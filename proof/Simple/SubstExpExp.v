
Require Import TyJudge.
Require Export Exp.

(** Substitution ****************************************************)
(* When we push new elements on the environment stack of an
   expression, we need to lift free indices in the expression 
   across the new elements.

   For example given: 
             t1, t0 |- 0 1 (\. 0 1 2) :: t3

   Pushing two more elements gives:
     t1, t0, ta, tb |- 2 3 (\. 0 3 4) :: t3
 *)
Fixpoint 
 liftX  (n:  nat) (* number of new elements pushed on environment *)
        (d:  nat) (* current binding depth in expression *)
        (xx: exp) (* expression to lift *)
        : exp
 := match xx with 
    |  XVar ix    
    => if bge_nat ix d
        (* var was pointing into env, lift it across new elems *)
        then XVar (ix + n)
        (* var was locally bound, leave it be *)
        else xx

    (* increase the depth as we move across a lambda *)
    |  XLam t1 x1
    => XLam t1 (liftX n (S d) x1)

    |  XApp x1 x2
    => XApp   (liftX n d x1) (liftX n d x2)
    end.


(* Substitute for the outermost binder in an expression. *)
Fixpoint
 subst' (d:  nat) (* current binding depth in expression *)
        (u:  exp) (* new expression to substitute *)
        (xx: exp) (* expression to substitute into *)
        : exp 
 := match xx with
    | XVar ix 
    => match compare ix d with
       (* Index matches the one we are substituting for. *)
       | EQ  => u
       
       (* Index was free in the original expression.
          As we've removed the outermost binder, also decrease this
          index by one. *)
       | GT  => XVar (ix - 1)

       (* Index was bound in the original expression. *)
       | LT  => XVar ix
       end

    (* Increase the depth as we move across a lambda.
       Also lift free references in the exp being substituted
       across the lambda as we enter it. *)
    |  XLam t1 x2
    => XLam t1 (subst' (S d) (liftX 1 0 u) x2)

    (* Applications *)
    |  XApp x1 x2 
    => XApp (subst' d u x1) (subst' d u x2)
 end. 


Definition  subst := subst' 0.
Hint Unfold subst.



(* Weakening Type Env in Type Judgement *****************************
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one.
 *)
Lemma type_tyenv_insert
 :  forall e ix x t1 t2
 ,  TYPE e x t1
 -> TYPE (insert ix t2 e) (liftX 1 ix x) t1.
Proof.
 intros. gen ix e t1.
 induction x; intros; simpl; inverts H; eauto.

 Case "XVar".
  breaka (bge_nat n ix).
  SCase "n >= ix".
   apply bge_nat_true in HeqX.
   apply TYVar. nnat. auto.

 Case "XLam".
  apply TYLam.
  rewrite insert_rewind. 
   apply IHx. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall e x t1 t2
 ,  TYPE  e         x            t1
 -> TYPE (e :> t2) (liftX 1 0 x) t1.
Proof.
 intros.
 assert (e :> t2 = insert 0 t2 e).
  simpl. destruct e; auto.
  rewrite H0. apply type_tyenv_insert. auto.
Qed.


(* Substitution of values in values. 
   Inductively, we must reason about performing substitutions at any
   depth, hence we must prove a property about (subst' d x2 x1) instead
   of the weaker (subst x2 x1) which assumes the substitution is taking
   place at top level.
 *)
Theorem subst_value_value_ix
 :  forall ix e x1 x2 t1 t2
 ,  get  e ix = Some t2
 -> TYPE e           x1 t1
 -> TYPE (drop ix e) x2 t2
 -> TYPE (drop ix e) (subst' ix x2 x1) t1.
Proof.
 intros. gen ix e x2 t1.
 induction x1; intros; simpl; inverts H0; eauto.

 Case "XVar".
  fbreak_compare.
  SCase "i = ix".
   rewrite H in H4. inverts H4. auto.

  SCase "n < ix".
   apply TYVar.
   rewrite <- H4.
    apply get_drop_above. auto.

  SCase "n > ix".
   apply TYVar.
   destruct n.
    false. omega.
    simpl. nnat. rewrite <- H4.
     apply get_drop_below. omega.

 Case "XLam".
  apply TYLam.
  rewrite drop_rewind.
  apply IHx1; auto.
   simpl. apply type_tyenv_weaken. auto.
Qed.


Theorem subst_value_value
 :  forall tenv x1 x2 t1 t2
 ,  TYPE (tenv :> t2) x1 t1
 -> TYPE tenv         x2 t2 
 -> TYPE tenv (subst x2 x1) t1.
Proof. 
 intros tenv x1 x2 t1 t2 Ht1 Ht2.
 lets H: subst_value_value_ix 0 (tenv :> t2).
  simpl in H. eauto.
Qed.
