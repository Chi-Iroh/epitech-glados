# Langage documentation PDP

## Types

Le PDP est un langage typé. Il faut spécifier le type d'une valeur à chaque fois qu'il n'est pas évident.

Voici la liste des types supportés par le langage :

- `int`: représente un nombre entier relatif
    - `uint`: représente un nombre entier positif
    - `char`: représente un entier compris entre 0 et 255

La base par défaut est la base décimale. Cependant une base différente (entre 2 et 36) peut être spécifiée avec un préfixe. Voici la liste des préfixes gérés :

| Préfixe | Base |
| -------- | ---- |
| `Ob` | 2 |
| `Oo` | 8 |
| `Od` | 10 |
| `Ox` | 16 |
| `O{n}` | n |

Chaque caractère ascii, noté entre `'`, est considéré comme un nombre entier de type `char` et dont sa valeur correspond à sa place sur la table ascii. Par exemple `'a'` est égal à `97`.

- `float`: représente un nombre à décimale flottante (le caractère utilisé pour la décimale est le point `.`)
- `bool`: représente une valeur booléenne (`#t` = true ; `#f` = false)
- `{a, b}`: représente un tuple de deux valeurs (les deux valeurs peuvent avoir des types distincts). `a` et `b` sont des types template.
- `[a]`: représente une liste de valeurs du même type (une liste s'initialise entre crochets `[]` et chaque élément de la liste doit être séparé d'une virgule `,`). `a` est un type template.
    - `string`: c'est un alias de `[char]`, représente une chaîne de caractères (une chaîne de caractère s'initialise entre guillemets `""`)
- `procedure`: représente un symbole invocable (variable, fonction, etc.)
    - `<(parameters_types...) => return_type>`: représente une fonction ou une lambda. `parameter_type` et `return_type` doivent être un type valide du PDP. Il peut il y avoir 0, 1 ou plusieurs `parameter_type`. Par exemple voilà une formulation valide : `<({a, integer} [a]) => a>`. *(Un type n'est pas une procédure. Un mot clé n'est pas une procédure non plus.)*
- `type`: représente un type ou une combinaison de types.

Le type d'une variable est strict. Pour transférer une valeur d'un type vers un autre il faut utiliser le builtin `cast`. *Pour plus d'informations à propos de `cast` consultez la liste des builtins.*

Il existe une valeur spéciale qui est compatible avec tous types : `NULL`. Elle sert à représenter une absence de valeur, une valeur erronée, etc.

Il existe un moyen de spécifier qu'une valeur peut être de plusieurs types avec le caractère `|`. Par exemple pour une valeur qui peut être `int` ou `float` il faut donner `int|float` comme type.

Plusieurs types mixtes possèdent un alias prédéfini :

| Alias | Combinaison de types |
| ----- | -------------------- |
| integer | `int\|unit\|char` |
| number | `integer\|float` |
| any | `number\|bool\|{a, b}\|[a]\|procedure\|type` |

## Syntaxe

La syntaxe du langage est basé sur les paranthèses à la manière du LISP. Il a plusieurs règles importantes à noter :

- Les "whitespace" sont interchangeable. Par exemple les formulations suivantes de define sont toutes équivalentes.

```lisp
(define name value)
```

```lisp
(
    define
    name
    value
)
```

```lisp
( define    name    value )
```

- Deux symboles (variables, fonction, etc.) ne peuvent pas avoir le même nom. Cela prend en compte les noms réservés par les builtins, les variables, les constantes et les types définies dans cette documentation.
- Le nom d'un symbole ne peut pas contenir un whitespace ou l'un des caractères suivant `,`, `;`, `"`, `'`, `(`, `)`, `[`, `]`, `-`, `|`, `<`, `>`, `=`, `&`, `!`, `:`, `/`, `%`, `*`, `+`, `~` et `^`.

### Variable

*`define` et `=` sont des mots clés.*

```lisp
(define name type value)
```

Il y a deux variables prédéfinies : `argc` & `argv`. Ces variables correspondent aux paramètres données au programme.

`argc` est un `int` qui correspond au nombre d'arguments du programme. Lorsqu'il n'y a aucun arguments donnés au programme `argc` vaut `0`.

`argv` est une `[string]` qui correspond à la liste des arguments donnés au programme. Lorsqu'il n'y a aucun arguments donnés au programme `argv` vaut `[]`.

### Définition de fonction

*`function` est un mot clé.*

```lisp
(function name (parameter::type ...) (body...) return_type)
```

Exemple :

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
```

Il y a des fonctions prédéfinies, des builtins en somme. Ceux-ci sont décrits plus bas dans ce document.

### Appel de fonction

```lisp
(name arguments...)
```

Le type de chaque arguments donnés à une fonction doit être identique ou plus restreint que le type du paramètre correspondant. Par exemple :

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
(f 1 0.1)
```

Le PDP gère la **récursivité**. Par exemple :

```lisp
(function factorial (x::integer)
    (if (eq? x 1)
        1
        (* x (factorial (- x 1)))
    )
)
```

Le PDP gère la **curryfication**. Par exemple :

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
((f 1) 0.1)
```

### Lambda

*`lambda` est un mot clé.*

```lisp
((lambda (parameter::type ...) (body...) return_type) arguments...)
```

Une lambda peut être assignée à une variable. Le résultat sera semblable à l'utilisation de `function`. Par exemple les deux lignes suivantes sont équivalentes :

```lisp
(function f (a::int|float b::int|float) (+ a b) int|float)
```

```lisp
(define f (lambda (a::int|float b::int|float) (+ a b) int|float))
```

### Condition

*`if` est un mot clé.*

```lisp
(if (condition) (vraie) (fausse))
```

La *condition* doit être une expression de type `bool`.

### Mots clés

Un "mot clé" en PDP est une "fonction" qui est traitée (au moins en partie) au parsing et non à la compilation ou au runtime. Un mot clé peut ne pas renvoyer de valeur.

| Opérateur | Prototype | Action |
| --------- | --------- | ------ |
| `=` | `define (var::procedure t::type value::any)` | Assignation d'une valeur/expression de type `t` à un symbole nommé `var`. Si l'action échoue sans lancer une erreur `var` vaudra `NULL`. `var` peut être un symbole préalablement défini ou non. |
|     | `import (lib::string)`            | Ajoute les définitions de la librairie `lib` au fichier courant. ATTENTION, import doit être seul sur sa ligne sinon une erreur est retourné |

### Builtins

#### Opérateurs numériques

| Opérateur | Prototype | Action |
| --------- | --------- | ------ |
| `+`   | `add (a::number b::number) number` | Performe $a + b$.      |
| `-`   | `sub (a::number b::number) number` | Performe $a - b$.      |
| `*`   | `mul (a::number b::number) number` | Performe $a \times b$. |
| `/`   | `div (a::number b::number) number` | Performe $a / b$. Si $b=0$ renvoie `NULL`.     |
| `%`   | `mod (a::number b::number) number` | Performe $a\:mod\:b$. Si $b=0$ renvoie `NULL`. |
| `**`  | `pow (a::number b::number) number` | Performe $a^b$.        |
| `v-`  | `sqrt (a::number) float`           | Performe $\sqrt{a}$. Si $a<0$ renvoie `NULL`.  |
| `!`   | `factorial (a::integer) uint`      | Performe $a!$. Si $a<0$ renvoie `NULL`.        |
| `+=`  | `add= (a::string b::number) bool`  | Applique la fonction `add` à `a` et `b` pour stocker le résultat dans `a`. Renvoie `#t` si l'action réussie, `#f` autrement. |
| `-=`  | `sub= (a::string b::number) bool`  | Applique la fonction `sub` à `a` et `b` pour stocker le résultat dans `a`. Renvoie `#t` si l'action réussie, `#f` autrement. |
| `*=`  | `mul= (a::string b::number) bool`  | Applique la fonction `mul` à `a` et `b` pour stocker le résultat dans `a`. Renvoie `#t` si l'action réussie, `#f` autrement. |
| `/=`  | `div= (a::string b::number) bool`  | Applique la fonction `div` à `a` et `b` pour stocker le résultat dans `a`. Renvoie `#t` si l'action réussie, `#f` autrement. |
| `%=`  | `mod= (a::string b::number) bool`  | Applique la fonction `mod` à `a` et `b` pour stocker le résultat dans `a`. Renvoie `#t` si l'action réussie, `#f` autrement. |
| `**=` | `pow= (a::string b::number) bool`  | Applique la fonction `pow` à `a` et `b` pour stocker le résultat dans `a`. Renvoie `#t` si l'action réussie, `#f` autrement. |

#### Opérateurs booléens

| Opérateur | Prototype | Action |
| --------- | --------- | ------ |
| `==`   | `eq (a::any b::any) bool`         | Perfome $a = b$.         |
| `!=`   | `neq (a::any b::any) bool`        | Perfome $a \neq b$.      |
| `<`    | `lw (a::number b::number) bool`   | Performe $a < b$.        |
| `>`    | `gt (a::number b::number) bool`   | Performe $a > b$.        |
| `<=`   | `lweq (a::number b::number) bool` | Performe $a <= b$.       |
| `>=`   | `gteq (a::number b::number) bool` | Performe $a >= b$.       |
| `!`    | `not (a::bool) bool`              | Renvoie l'opposé de `a`. |
| `&&`   | `and (a::bool b::bool) bool`      | Performe $a$ AND $b$.    |
| `\|\|` | `or (a::bool b::bool) bool`       | Performe $a$ OR $b$.     |
| `!&`   | `nand (a::bool b::bool) bool`     | Performe $a$ NAND $b$.   |
| `!\|`  | `nor (a::bool b::bool) bool`      | Performe $a$ NOR $b$.    |
| `:\|`  | `xor (a::bool b::bool) bool`      | Performe $a$ XOR $b$.    |
| `!:`   | `xnor (a::bool b::bool) bool`     | Performe $a$ XNOR $b$.   |

#### Opérateurs binaires (sur entier)

| Opérateur | Prototype | Action |
| --------- | --------- | ------ |
| `&`  | `band (a::integer b::integer) integer`   | Performe $a$ AND $b$. |
| `\|` | `bor (a::integer b::integer) integer`    | Performe $a$ OR $b$.  |
| `~`  | `bnot (a::integer b::integer) integer`   | Performe $a$ NOT $b$. |
| `^`  | `bxor (a::integer b::integer) integer`   | Performe $a$ XOR $b$. |
| `<<` | `lshift (a::integer b::integer) integer` | Performe un décalage vers la gauche de `b` sur `a`. |
| `>>` | `rshift (a::integer b::integer) integer` | Performe un décalage vers la droite de `b` sur `a`. |

#### Librairie tuple

| Prototype | Action |
| --------- | ------ |
| `left (t::{a, b}) a`      | Renvoie le membre de gauche de `t`. |
| `right (t::{a, b}) b`     | Renvoie le membre de droite de `t`. |
| `swap (t::{a, b}) {b, a}` | Inverse les membres de gauche et de droite de `t`. |

#### Librairie list

| Prototype | Action |
| --------- | ------ |
| `len (l::[a]) uint`                       | Renvoie la longueur de `l`.         |
| `concat (la::[a] lb::[a]) [a]`            | Concatène deux listes `la` et `lb`. |
| `find (l::[a] predica::{(a) => bool}) a`  | Applique le `predica` à chaque élément de `l` et renvoie le premier élément où le `predica` renvoie `#t`.   |
| `split (l::[a] i::uint) {[a], [a]}`       | Divise la liste `l` en deux listes à partir du ième. Le ième élément sera le dernier de la liste de gauche. |
| `first (l::[a]) a`                        | Renvoie le premier élément de `l`. Si `l` est vide renvoie `NULL`. |
| `last (l::[a]) a`                         | Renvoie le dernier élément de `l`. Si `l` est vide renvoie `NULL`. |
| `pushback (l::[a] item::a) [a]`           | Ajoute `item` à la fin de `l`.      |
| `pushfront (l::[a] item::a) [a]`          | Ajoute `item` au début de `l`.      |
| `get (l::[a] i::uint) a`                  | Renvoie le ième élément de `l`.     |
| `reverse (l::[a]) [a]`                    | Inverse l'ordre de `l`.             |

#### Librairie maths

| Constante | Valeur |
| --------- | ------ |
| `pi` | `3.14159265` |
| `e`  | `2.71828182` |

| Prototype | Action |
| --------- | ------ |
| `exp (a::number) number`           | Performe $e^a$.              |
| `ln (a::number) number`            | Performe $ln(a)$.            |
| `max (a::number b::number) number` | Renvoie le plus grand parmi `a` et `b`. |
| `min (a::number b::number) number` | Renvoie le plus petit parmi `a` et `b`. |
| `cos (a::number) float`            | Performe $cos(a)$.           |
| `acos (a::number) float`           | Performe $acos(a)$.          |
| `cosh (a::number) float`           | Performe $cosh(a)$.          |
| `sin (a::number) float`            | Performe $sin(a)$.           |
| `asin (a::number) float`           | Performe $asin(a)$.          |
| `sinh (a::number) float`           | Performe $sinh(a)$.          |
| `tan (a::number) float`            | Performe $tan(a)$.           |
| `atan (a::number) float`           | Performe $atan(a)$.          |
| `tanh (a::number) float`           | Performe $tanh(a)$.          |
| `ceil (a::float) float`            | Arrondie `a` au spérieur.    |
| `round (a::float) float`           | Arrondie `a` au plus proche. |
| `trunc (a::float) float`           | Arrondie `a` à l'entier.     |
| `floor (a::float) float`           | Arrondie `a` à l'inférieur.  |

## Commentaires dans le code

Il est possible d’ajouter des commentaires dans le code grâce à la syntaxe suivante : `--`.

Un commentaire débute avec `--` et se termine soit :
- à la fin de la ligne,
- soit lorsque la syntaxe `--` apparaît à nouveau dans la même ligne.

## Exemple

```
-- Ceci est un commentaire, il ne sera pas exécuté --

Ceci n'est pas un commentaire

-- Ceci est un commentaire mais ceci est du code : -- (+ 4 5)
```

### Explications :

1. Le premier commentaire est ignoré par l’interpréteur.
2. La deuxième ligne est du code invalide.
3. La troisième ligne contient :
   - un commentaires, qui commence et se termine avec `--`.
   - Une expression fonctionnelle `( + 4 5 )`, qui est interprétée comme du code.

Utilisez cette fonctionnalité pour clarifier votre code sans affecter son exécution.
