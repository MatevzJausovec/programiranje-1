set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

def vsota_prvih : Nat → Nat := 
  fun n => 
    match n with
    | Nat.zero => 0
    | n + 1 => n + 1 + vsota_prvih (n)

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) := by
  intro n
  induction n with
  | zero => rfl
  | succ n hn => 
    rw [vsota_prvih, Nat.left_distrib, hn]
    simp [Nat.left_distrib, Nat.right_distrib, Nat.two_mul]
    rw [Nat.add_comm]
    repeat rw [Nat.add_assoc]
    rw [Nat.add_comm 1 (n + 1)]

theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 := by
  intro n
  have h1 : 0 < 2 := by trivial
  apply Eq.symm
  apply Nat.div_eq_of_eq_mul_left
  exact h1
  apply Eq.symm
  rw [Nat.mul_comm]
  apply gauss

/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n :=
  fun {A : Type} {m : Nat} (xs : Vektor A m) =>
    match xs with
    | .prazen => Vektor.prazen
    | .sestavljen x xs' => stakni (obrni xs') (Vektor.sestavljen x (Vektor.prazen))

def glava : {A : Type} → {n : Nat} → Vektor A (n + 1) → A :=
  fun (Vektor.sestavljen x _) => x

def rep : {A : Type} → {n : Nat} → Vektor A (n + 1) → Vektor A n :=
  fun (Vektor.sestavljen _ xs) => xs

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  intros A P Q h1 h2 x
  apply h1
  apply h2

theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  intros A P Q
  constructor
  intros h p x
  apply h
  exact p
  intros h x p
  apply h
  exact p

theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by
  intros  G P g
  have lem : (∀ (x : G), P x) ∨ (¬ ∀ (x : G), P x) := by apply Classical.em
  cases lem 
  case inl h1 =>
    have proof1 : P g → ∀ (x : G), P x := by 
      intros trditev x
      apply h1
    exact Exists.intro g proof1
  case inr h2 =>
    have h2' : ∃ (x : G), ¬ P x:= by apply Classical.not_forall.mp h2
    let ⟨w, hw⟩ := h2'
    have proof2 : P w → ∀ (x : G), P x := by
      intro a
      trivial
    apply Exists.intro w proof2

/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []

theorem zrcali_1 : {A : Type} → (tl : Drevo A) → (a : A) → (td : Drevo A) → 
  zrcali (tl.sestavljeno a td) = (zrcali td).sestavljeno a (zrcali tl) := by 
  intros
  trivial

theorem visina_1 : {A : Type} → (tl : Drevo A) → (a : A) → (td : Drevo A) → 
  visina (tl.sestavljeno a td) = 1 + max (visina tl) (visina td) := by
  intros
  trivial

theorem zrcali_zrcali :
  {A : Type} → (t : Drevo A) →
  zrcali (zrcali t) = t := by
  intros A t
  induction t with
  | prazno => rfl
  | sestavljeno  tl a td h1 h2 => 
    rw [zrcali_1, zrcali_1, h1, h2]
     
theorem visina_zrcali :
  {A : Type} → (t : Drevo A) →
  visina (zrcali t) = visina t := by
  intros A t
  induction t with
  | prazno => rfl
  | sestavljeno tl a td h1 h2 =>
    rw [zrcali_1, visina_1, visina_1, h1, h2, Nat.max_comm]
    

theorem elementi_elementi'_2 : {A : Type} → (t : Drevo A) → (l : List A) → 
  (elementi t = elementi' t) ∧  (elementi'.aux t (l) = (elementi t) ++ l) := by
  intros A t
  induction t with 
  | prazno => 
    intro l
    trivial
  | sestavljeno tl a td h1 h2 =>

    intro l
    have h1_r : ∀ (l : List A), elementi'.aux tl l = elementi tl ++ l := by 
      intro ls
      let h1' := h1 ls
      exact h1'.right
    have h2_r : ∀ (l : List A), elementi'.aux td l = elementi td ++ l := by
      intro ls
      let h2' := h2 ls
      exact h2'.right
    have h1_l : elementi tl = elementi' tl := by 
      let h1' := h1 l
      exact h1'.left
    have h2_l : elementi td = elementi' td := by
      let h2' := h2 l
      exact h2'.left

    have enakost2 : (l : List A) → elementi'.aux (tl.sestavljeno a td) l = elementi (tl.sestavljeno a td) ++ l := by
      intro l
      calc
        elementi'.aux (tl.sestavljeno a td) l
        _ = elementi'.aux tl (a :: elementi'.aux td l) := by trivial
        _ = (elementi tl) ++ (a :: elementi'.aux td l) := by rw [h1_r]
        _ = (elementi tl) ++ a :: (elementi td ++ l) := by rw [h2_r]
        _ = (elementi tl) ++ ([a] ++ (elementi td ++ l)) := by trivial
        _ = (elementi tl) ++ ([a] ++ (elementi td)) ++ l := by simp [List.append_assoc]
        _ = ((elementi tl) ++ a :: (elementi td)) ++ l := by trivial
        _ = elementi (tl.sestavljeno a td) ++ l := by trivial

    have enakost1 : elementi (tl.sestavljeno a td) = elementi' (tl.sestavljeno a td) := by 
      calc
        elementi (tl.sestavljeno a td)
      _ = elementi tl ++ a :: elementi td := by trivial
      _ = elementi tl ++ a :: elementi' td := by rw [h2_l]
      _ = elementi'.aux tl (a :: elementi' td) := by rw [h1_r]
      _ = elementi' (tl.sestavljeno a td) := by trivial
    constructor
    trivial
    apply enakost2

theorem elementi_elementi' :
  {A : Type} → (t : Drevo A) →
  elementi t = elementi' t := by 
  intros A t
  let (l : List A) := []
  let h := elementi_elementi'_2 t l
  exact h.left
