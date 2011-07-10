
import DDC.War.Options
import DDC.War.Way
import DDC.War.Config
import DDC.War.Job
import DDC.War.JobCreate
import DDC.War.JobDispatch
import DDC.War.Result
import DDC.War.Pretty
import Util.Options
import Util.Options.Help
import BuildBox
import System.Environment
import System.Directory
import System.IO
import System.Random
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Exception
import Data.List
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Traversable	as Seq


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions warOptions args
	let help		= makeOptionHelp 30 ["all"] warOptions 

	-- Print command usage if asked for.
	when (elem OptHelp options)
	 $ do	putStrLn $ help
		exitSuccess

	-- Print errors if there are any.
	when (not $ null errs)
	 $ do	putStrLn $ (concat $ intersperse "\n" errs) 
		putStrLn $ help
		exitFailure

	-- Load war config from the cmd line options
	let config = loadConfig options
		
	-- All the starting test directories from the command line.
	testDirs
		<- mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
		$  [dirs | OptTestDir dirs <- configOptions config]

	-- Trace all the files reachable from these directories.
	testFilesRaw
		<- liftM (join . Seq.fromList)
		$  mapM traceFilesFrom testDirs
		
	-- Canonicalize all the paths and put them in a set (which sorts them)
	testFilesSet
		<- liftM (Set.fromList . Seq.toList)
		$  Seq.mapM canonicalizePath
		$  testFilesRaw

	let testFilesSorted
		= filter (not . isInfixOf "skip-")	-- skip over skippable files.
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")	-- don't look at srcs in copied build dirs.
		$ Set.toList testFilesSet

	-- Create test chains based on the files we have.
	let ways'
		= case configWays config of
		   []	-> [Way "std" [] []]
		   ways	-> ways

	let jobChains :: [[Job]]
	    jobChains
		= concat
		$ map (filter (not . null))
		$ [ map (\way -> createJobs config way testFilesSet file) ways'
			| file <- testFilesSorted]

	-- Channel for threads to write their results to.
	(chanResult :: ChanResult)
		<- atomically $ newTChan

	-- Run all the chains.
	runJobChains config chanResult jobChains

	return ()

-------------------------------------------------------------------------------
data JobResult
 	= JobResult 
	{ _jobResultChainIx	:: Int
	, _jobResultJobIx	:: Int
	, _jobResultJob		:: Job
	, _jobResultResults	:: [Result] }

type ChanResult
	= TChan JobResult
	

-- | Run some job chains.
runJobChains 
	:: Config 	-- ^ war configuration
	-> ChanResult	-- ^ channel to write job results to
	-> [[Job]]	-- ^ chains of jobs
	-> IO ()

runJobChains config chanResult jcs
 = do	
	-- Count the total number of chains for the status display.
	let chainsTotal	= length jcs
	
	-- Fork a gang to run all the job chains.
	gang	<- forkGangActions (configThreads config)
	 	$ zipWith (runJobChain config chanResult chainsTotal)
			[1..]
			jcs

	-- Fork the gang controller that manages the console and handles
	-- user input.
	varControllerDone	<- newEmptyMVar
	forkIO	$ controller config gang chainsTotal chanResult
		`finally` (putMVar varControllerDone ())

	-- Wait until the gang is finished running chains.
	waitForGangState gang GangFinished

	-- Wait until the controller to finished
	takeMVar varControllerDone
	
	return ()


controller 
	:: Config
	-> Gang
	-> Int		-- ^ total number of chains
	-> ChanResult	-- ^ channel to receive results from
	-> IO ()

controller config gang chainsTotal chanResult
 = go_start
 where	
	-- See if there is an input on the console.
	go_start 
	 =  hReady stdin >>= \gotInput
	 -> if gotInput
		then go_input
		else go_checkResult
	
	go_input
	 = do	putStrLn "Interrupt. Waiting for running jobs (CTRL-C kills)..."
	 	flushGang gang
			
	go_checkResult
	 =  (atomically $ isEmptyTChan chanResult) >>= \isEmpty
	 -> if isEmpty
	     then do
		gangState	<- getGangState gang
		if gangState == GangFinished
		 then	return ()
		 else do
			threadDelay 100000
			go_start

	     else do
		jobResult	<- atomically $ readTChan chanResult
		controller_ok config chainsTotal jobResult
		go_start


controller_ok config chainsTotal (JobResult chainIx jobIx job results)
 = do	-- Display the result.
	dirWorking	<- getCurrentDirectory
	let useColor	= not $ configBatch config
	let width	= configFormatPathWidth config

	putStrLn 
		$ render 
		$ parens (padR (length $ show chainsTotal)
				(ppr $ chainIx) 
				<> text "."
				<> ppr jobIx
				<> text "/" 
				<> ppr chainsTotal)
		<> space
		<> pprJobResult width useColor dirWorking job results

	hFlush stdout


-- | Get a unique(ish) id for this process.
--   The random seeds the global generator with the cpu time in psecs, which should be good enough.
getUniqueId :: IO Integer
getUniqueId
 	= randomRIO (0, 1000000000)	

-- | Run a job chain, printing the results to the console.
--   If any job in the chain fails, then skip the rest.
runJobChain 
	:: Config		-- ^ war configuration
	-> ChanResult		-- ^ channel to write job results to
	-> Int			-- ^ total number of chains
	-> Int			-- ^ index of this chain
	-> [Job]		-- ^ chain of jobs to run
	-> IO ()

runJobChain config chanResult chainsTotal chainNum chain
 = do	uid		<- getUniqueId
	let state	= (buildStateDefault uid "/tmp")
			{ buildStateLogSystem
				= if configDebug config
					then Just stderr
					else Nothing }
	
	runBuildWithState state
 		$ zipWithM_ (runJob config chanResult chainNum)
			[1..]
			chain

	return ()

-- | Dispatch a single job of a chain.
runJob
	:: Config 		-- ^ war configuration
	-> ChanResult		-- ^ channel to write results to
	-> Int			-- ^ index of this chain
	-> Int			-- ^ index of this job of the chain
	-> Job			-- ^ the job to run
	-> Build ()

runJob config chanResult chainNum jobNum job
 = do	
	-- Run the job
	results		<- dispatchJob job
	
	-- Push the results into the channel for display
	io $ atomically $ writeTChan chanResult 
		(JobResult chainNum jobNum job results)
		
	return ()
