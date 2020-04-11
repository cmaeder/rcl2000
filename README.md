# RCL 2000

## Building

Compilation is done using `stack` from
[http://haskellstack.org](http://haskellstack.org). My current stack
version is `2.1.3`.

```
stack build --pedantic
stack run examples/AhnSandhuPaper2000.rcl
```

Via the file [stack.yaml](stack.yaml) the compilation uses the latest
Glasgow haskell compiler version. Currently this is `ghc-8.8.3` as
specified by the resolver version `lts-15.6`, see
[https://www.stackage.org/](https://www.stackage.org/). Compilation
should also succeed with older ghc versions (down to `ghc-7.10.2` of
`lts-3.22`). Apart from the `--pedantic` flag the haskell sources are
kept clean using the tools
[hlint](https://github.com/ndmitchell/hlint) and
[scan](https://hackage.haskell.org/package/scan).

## Motivation

This work was inspired by
[Karsten Sohr](http://www.informatik.uni-bremen.de/~sohr/) who pointed
me to the paper
[Role-based Authorization Constraints Specification][1]
from Gail-Joon Ahn and Ravi Sandhu published in 2000 as well as by my
current project work regarding access control as part of (seaport)
security and my former
[haskell programming work](https://github.com/spechub/Hets) on
[Hets](http://hets.eu/).

The *R*ole-based *C*onstraints *L*anguage RCL 2000 is supposed to
provide a foundation for systematic studies and evaluations of
role-based authorization constraints. After twenty years this
implementation aims to provide the envisaged front-end that can be
realistically used by security policy designers.

## Implementation

The abstract syntax tree of RCL 2000 as given in the [paper][1] is
reflected by the haskell data types in [Ast](src/Rcl/Ast.hs). We
denote the function terms over sets simply as sets with type `Set` and
avoid the words term, token and expression. Statements have the type
`Stmt` and are actually boolean formulas. For these recursive
data types handy folding functions are provided to shorten repetitive
recursive function definitions.

A parser for a list of statements is implemented in
[Parse](src/Rcl/Parse.hs).  The code uses the (`parsec1` subset of the
old) haskell `parsec` parser combinator library without separate
scanner. The parser accepts ascii, latex and unicode variants of the
syntax. The (currently) accepted function names are `user`, `roles`,
`sessions`, `permissions`, `operations`, and `objects`. Note that
`user` is singular and `objects` is plural. Only the `user` function
applied to a single session yields a single user. In all other cases
sets are returned, although `objects` returns a singleton set if
applied to a single permission. Note that `operations` is a binary
function and the only one (apart from set operations like union and
intersection). The application of unary functions can be written
*with* (as in the original [paper][1]) or *without parentheses* (like in
haskell): "`AO(OE(CR))`" versus "`AO OE CR`".

As in the [paper][1] the functions `user` and `roles` are overloaded
and functions `roles` and `permissions` have "`*`" variants that
consider a role hierarchy. (The supported latex suffix is "`^{*}`".)
Also the general notational device for set-valued functions is
respected. This allows whole sets instead of single values as
arguments. (Only the `user` function for sessions is not set-valued.)

Proper type checking is implemented in [Type](src/Rcl/Type.hs) using a
simple state monad (from haskell package `mtl`). The [paper][1]
mentions `CR`, `CP`, and `CU` as sets of conflicting sets of roles,
permissions and users, respectively. However, these sets are not built
in but user defined and must therefore be supplied separately. The
types of such names are currently read from the file
[examples/types.txt](examples/types.txt) if the `-t` option is
supplied via the command line. An alternative file path could be
appended to the `-t` option. The command line interface for the binary
is defined in [Cli](src/Rcl/Cli.hs).

The module [Print](src/Rcl/Print.hs) allows to output parsed statements
to ascii, latex or unicode text with or without redundant parentheses.
Pretty printed output can be successfully re-parsed, i.e. round tripped.
Printing uses the (good'n old) `pretty` haskell package (see
`dependencies` in [package.yaml](package.yaml)).

In [Reduce](src/Rcl/Reduce.hs) the reduction to RFOPL and inverse
(re-) construction from RFOPL is implemented. The construction is only
needed for test purposes. Reduction only works for type correct
statements and is a prerequisite for the translation
[ToOcl](src/Rcl/ToOcl.hs) to OCL invariants for the [USE-Tool][2]
(not described in the [paper][1]).

```
stack run -- -t -o. Stmts.rcl
use Stmts.use
```

The above call, where "`stack run --`" could be the created binary
`rcl200-exe`, would produce the file `Stmts.use` from the statements in
the input file `Stmts.rcl` (look into the [examples](examples)
directory for `.rcl` files). Note the `-t` option for proper type
checking and also be aware of the file [use/RBAC.use](use/RBAC.use)
that is always the beginning of any created `.use` file. The default
output directory for `.use` files created by the `-o` option is
[use](use). The option `-o.` puts these files into the current
directory ["."](.).

Within the [USE-Tool][2] concrete object diagrams could now be created and
the generated OCL invariants for a single `RBAC` object could be
validated or refuted.

Yet, the implementation does not stop here. It allows to read in a full
configuration (see below)

## Extensions and Limitations

The parser is more liberal than described in the [paper][1]:
- some parentheses may be omitted.
- set names can start with any unicode letter. Further characters may
  be any alphanum or the underscore. Other characters may be easily
  allowed if they are not used elsewhere.
- The minus sign `-` can be used directly to remove an element from a
  set: "`AO(S)`" is the same as "`S-{OE(S)}`" or "`S - OE S`" as also
  the curly braces are optional. The second argument of minus `-` must
  be an element of the first argument. (The first part of the reduction
  is in fact the replacement of `AO` using `-` and `OE`.)

The type checker is as liberal as described in the [paper][1]. Many
elements are regarded as singleton sets to ensure proper typing. The
`roles` function is even more overloaded than in the [paper][1]. Apart
from users `U`, permissions `P` and sessions `S` also roles `R` can be
arguments of the `roles` function:

- roles : R -> 2^R, roles(r) = { j | j < r }
- roles* : R -> 2^R, roles*(r) = { s | r < s }

So `roles` can be used to compute all proper junior roles and `roles*`
for the proper senior roles. With these two functions the `*` variants
of other functions are strictly no longer necessary but supported:

- roles*(u : U) = roles(roles(u))
- roles*(s : S) = roles(roles(s))
- roles*(p : P) = roles\*(roles(p))
- permissions*(r : R) = permission(roles(r))

It should be noted that also the `user` and `operations` functions
must consider the role hierarchy as this is not mentioned (or wrong)
in the [paper][1].

- user(r : R) = { u | (u, r) in UA }
- user*(r : R) = user(roles\*(r)) = { u | exist s >= r . (u, s) in UA }
- operations*(r : R, obj : OBJ) = operations(roles(r), obj)
  = { op | exist j <= r . (op, obj, j) in PA }

The transitive closure of a role hierarchy is computed from an input
file and the (minimal) transitive reduction is written out as `.soil`
file for the [USE-Tool][2].

Activated roles of sessions are properly checked against the
*authorized* roles of a session's user by also considering a role
hierarchy.

Concrete conflicting sets, like `CR`, `CP`, or `CU` can be read
from a file, although all subsets must be named explicitely.

## Usage

```
find . -name rcl2000-exe
# find the binary and put it into your PATH
# or use "stack run --" or "stack exec rcl2000-exe --" instead of "rcl2000-exe"
rcl2000-exe -o. Stmts.rcl
use Stmts.use Stmts.soil
```

The above call relies on the files [rh, ua, pa, s and sets](examples)
from the [examples](examples) directory. The default file extension is
`.txt`, but could be changed using the `-x` option.

[rh](examples/rh.txt) contains roles, usually one role on a
separate line. Several roles on one line define a senior role and all
(direct) junior roles of this senior role.

[ua](examples/ua.txt) introduces all users with there (statically)
assigned roles. This defines the user-to-role assignment or a single
user if there is only one name in a line.

[pa](examples/pa.txt) defines the permission-to-role
assignment. Each permission is given by two leading words of a line.
The first word is the operation and second the object of the
permission.  The internal name of a permission will be simply the
operation joined with the object by an (additional) underscore.

[s](examples/s.txt) contains users sessions. A line starts with a
session id and a user name and all remaining words are the associated
*activated* roles. Note that the activated roles must be a subset of
the *assigned* roles of the user.

[sets](examples/sets.txt) contains user defined sets of known
entities like roles, users, etc. The leading word in a line must a new
name, all following words must be known and they must be of the same
type to form a new homogeneous set with the given elements. This way
sets of sets can be defined to describe conflicts like `CR`. The
actual types of user defined names is no longer taken from the
[types](examples/types.txt) file but derived from these set
definitions.

When the above files are read some sanity checks are performed that
are reported. The hopefully informative messages should be
inspected. (Run time errors that point to source locations, should be
reported with instructions to reproduce them.)

Provided the generated `.use` and `.soil` files are successfully
loaded by [use][2], the tool buttons to show class- and object
diagrams as well as class invariants should work. The generated
structure (menu `State`) should also be fine.

The class invariants generated from the input file should validate in
the same way as the internal evaluation that is triggered using the
`-e` option.

The `-i` option sends rcl2000 into interactive mode that can be
terminated by entering `q` or pressing `Ctrl-C` (interrupt) or
`Ctrl-D` (end of input). You may also try to enter `h` for help or
some set expressions. This feature is still provisional.

The `-h` option displays a usage message.

[1]: https://dl.acm.org/doi/10.1145/382912.382913
[2]: https://sourceforge.net/projects/useocl
