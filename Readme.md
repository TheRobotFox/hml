- [HML Solver and $\LaTeX$ Export](#orgc465ca0)
- [Usage](#org6902de2)
  - [Define LTS](#org2ebb854)
  - [Defining System of Equations](#orgb123cbd)
  - [Create n-nested recursive System](#orgc92affd)
  - [Solve System on LTS](#org8e55c9c)


<a id="orgc465ca0"></a>

# HML Solver and $\LaTeX$ Export

This is a simple tool, which allows solving n-nested-recursive Systems on LTS and export the required calculations as $\LaTeX$ Code. The exported Script uses following macros to model HML-Fromula


<a id="org6902de2"></a>

# Usage


<a id="org2ebb854"></a>

## Define LTS

Define Processlabels, Actions and Variables

```haskell
data Proc = P0|P1|P2|P3 deriving (Eq, Ord, Show)
data Act = A|B                          deriving (Eq, Ord,Show)
data Variables = X|Y|Z                      deriving (Eq, Show, Ord)
```

Proc and Act need both instances of `Eq,Ord` since the Algorithm uses `Data.Set` to solve equations. The `Show` instance is used to render objects when producing a Formular. One might use custom `Show` instances instead to allow for pretty $LaTeX$ export. It's also possible to define the Processlabels as `data Proc = Proc String`, which would ease the implementation of pretty $\LaTeX$ printing, though this apporach is discouraged since it allows miskates like misspellings when defining HML Formulars. Same applies to `Act` and `Variables`.

The `Lib` Module exports definitions for `Transition p a` which model *T* part of an LTS. Using the function `ltsFromTrans :: Set (Transition p a) -> LTS p a` allows infering a whole LTS from a Set of Transitions.

```haskell
trans = fromList [
  Transition P1 A P0,
  Transition P1 B P3,
  Transition P3 A P0,
  Transition P4 B P1,
  Transition P1 A P2,
  Transition P2 B P2,
  Transition P3 B P3]

lts = ltsFromTrans trans
```


<a id="orgb123cbd"></a>

## Defining System of Equations

Let's asume we want to solve a System of Equation like follows

\begin{align*}
 F = Z\hspace{0.5cm}\mathrm{with} \hspace{0.5cm} & Z \overset{\mathrm Min}=([A]Y \land <B>X)\lor [B]Z\\
           &Y \overset{\mathrm Max}= <B>X \land <\mathrm{Act}>Y\\
      &X \overset{\mathrm Max}= <B> t\!\!\!t\land <A>Y \land [B]X
\end{align*}

where $V$ is our Set of Variables and $F\in \mathcal M_{V}$ is a HML Formular.

We want to define a *Declaration* $f:V\to \{\mathrm{Max},\mathrm{Min}\}\times \mathcal M_{V}$, mapping Variables to their Definitions. This might look like this

```haskell
decl :: Declaration Act Variable
decl Z = Min $ (All [A] (Var Y) `And` Ex [B] (Var X)) `Or` All [B] (Var Z)
decl Y = Max $ Ex [B] (Var X) `And` Ex acts (Var Y)       where acts = Data.Set.toList . act $ lts
decl X = Max $ Ex [B] TT `And` Ex [A] (Var Y) `And` All [B] (Var X)
```

where we use the `act` record accessor to retrieve all Actions defined in our LTS. `Min` and `Max` are constructors for the `Fixpiont a` datatype.


<a id="orgc92affd"></a>

## Create n-nested recursive System

The fucntion `makeSystem :: HML a v -> Declaration a v -> HMLEquation a v` will transform the declaration into a n-nested-recursive System if possible. When the declaration is malformed, this means it contains mutually recursive Variables of varied Fixpoint-Types the function will return `Nothing`.


<a id="org8e55c9c"></a>

## Solve System on LTS

At last you'd use the function `solveSystem :: LTS p a -> HMLEquation a v -> String` which solves the whole System and produces the $\LaTeX$ Code documenting the calculation steps as a String. The Result might look like this
