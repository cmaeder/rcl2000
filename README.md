# RCL 2000

## Building

Compilation is done using `stack` from
[http://haskellstack.org](http://haskellstack.org). My current stack
version is `2.3.1`.

    stack build --pedantic
    stack run examples/AhnSandhuPaper2000.rcl

Via the file [stack.yaml](stack.yaml) the compilation uses the latest
Glasgow haskell compiler version. Currently this is `ghc-8.8.3` as
specified by the resolver version, see
[https://www.stackage.org/](https://www.stackage.org/). Compilation
should also succeed with older ghc versions (down to `ghc-7.10.2` with
suitable packages). Apart from the `--pedantic` flag the haskell
sources are kept clean using the tools
[hlint](https://github.com/ndmitchell/hlint), and
[scan](https://hackage.haskell.org/package/scan). Under windows you
should use the installer for
[stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). For
properly displaying unicode characters you may need to choose a proper
font of your command console. The haskell package
[code-page](https://github.com/RyanGlScott/code-page) is used to
switch to the unicode code page 65001.

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
*with* (as in the original [paper][1]) or *without parentheses*:
"`AO(OE(CR))`" versus "`AO OE CR`". (This is only similar to Haskell
as this juxtaposition in RCL is right-associative.)

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

    stack run -- -t -o. Stmts.rcl
    use Stmts.use

The above call, where "`stack run --`" could be the created binary
`rcl200-exe`, would produce the file `Stmts.use` from the statements
in the input file `Stmts.rcl` (look into the [examples](examples)
directory for `.rcl` files). The input file `Stmts.rcl` may not exist,
but the output file `Stmts.use` will be created and would overwrite an
existing file. Without input statements no invariants would be
generated, though. Note the `-t` option for proper type checking and
also be aware of the file [use/RBAC.use](use/RBAC.use) that is always
the beginning of any created `.use` file. The default output directory
for `.use` files created by the `-o` option is [use](use). The option
`-o.` puts these files into the current directory ["."](.).

Within the [USE-Tool][2] concrete object diagrams could now be created and
the generated OCL invariants for a single `RBAC` object could be
validated or refuted.

Yet, the implementation does not stop here. It allows to read in a full
configuration as described below under the [Usage heading](#usage).

## Extensions

The parser is more liberal than described in the [paper][1]:

- some parentheses may be omitted.

- set names can start with any unicode letter. Further characters may
  be any alphanum or the underscore. Other characters may be easily
  allowed if they are not used elsewhere.

- the minus sign `-` can be used for set difference: "`AO(S)`" is the
  same as "`S-{OE(S)}`" or "`S - OE S`" as the curly braces will be
  derived by type checking. (The first part of the reduction is in
  fact the replacement of `AO` using `-`, curly braces and `OE`.)

- the function `user` may also be written `users` (plural) and
  `objects` as `object` (singular).

- explicit `juniors` and `seniors` functions with and without `*` as
  well as other partly overloaded functions have been added as
  described below.

- user defined sets may be overloaded and type annotated for
  disambiguation. "`U`" has type "`Us`" and may be written as
  "`U:Us`". Names are not restricted, even keywords or function names
  may be used as names of sets. Sometimes annotations, parentheses,
  commas, or semicolons are needed for proper parsing.

- (non-empty) sets can be constructed on the fly using curly
  braces. (In fact singleton sets are constructed during type checking
  and reduction.) A separating comma between elements of sets is only
  optional because the end of a set and the start of a second set can be
  recognized if no function names are used as base sets.

- a `UP ⊆ U x P` history relation was added with functions
  `executions : U -> 2^P` and `accessors : P -> 2^U`:

        executions(u : U) = { p | (u, p) in UP }
        accessors(p : P) = { u | (u, p) in UP }

- A statement may be preceded by `let` definitions:

        let v_1 = s_1 ; ... ; v_n = s_n in stmt

The variables `v_i` shadow user defined sets (or equally named
variables `v_j` with `j < i`). A variable `v_j` may be used in any set
`s_i` with `j < i` or in the actual statement `stmt`. The semantics of
`let` is a simple substitution of all variables from right to left in
`stmt`. A variable `v_i` may be type-annotated which is identical to a
type annotation of the set `s_i`. (The semicolons in `let` definitions
are also optional.)

The type checker is as liberal as described in the [paper][1]. Many
elements are turned to singleton sets to ensure proper typing. The
following reflexive transitive closure functions have been added:

        juniors* : R -> 2^R, juniors*(r) = { j | j <= r }
        seniors* : R -> 2^R, seniors*(r) = { s | r <= s }

So `juniors*` can be used to compute all junior roles and `seniors*` for
the senior roles including the argument role. With these two functions
the `*` variants of other functions are strictly no longer necessary,
though still supported:

        roles(u : U) = { r | (u, r) in UA }
        roles(s : S) = defined activated roles of session s
        roles(p : P) = { r | (p, r) in PA }
        roles*(u : U) = juniors*(roles(u))
        roles*(s : S) = juniors*(roles(s))
        roles*(p : P) = seniors*(roles(p))
        permissions(r : R) = { p | (p, r) in PA)
        permissions*(r : R) = permission(juniors*(r))

The `permissions` function is additionally overloaded and accepts user
and session arguments:

        permissions(u : U) = permissions(roles(u))
        permissions(s : S) = permissions(roles(s))
        permissions*(u : U) = permissions*(roles(u))
        permissions*(s : S) = permissions*(roles(s))

It should be noted that also the `user` and `operations` functions
must consider the role hierarchy as this is not mentioned (or wrong)
in the [paper][1].

        user(r : R) = { u | (u, r) in UA }
        user*(r : R) = user(seniors*(r)) = { u | exist s >= r . (u, s) in UA }
        operations(r : R, obj : OBJ) = { op | (op, obj, r) in PA }
        operations*(r : R, obj : OBJ) = operations(juniors*(r), obj)
            = { op | exist j <= r . (op, obj, j) in PA }

The `user*` function also considers users that may have a role's
permissions due to their assignment to senior roles. The first
argument of `operations` may also be a permission or a user:

        operations(p : P, obj : OBJ) = { op | p = (op, obj) }
        operations(r : R, obj : OBJ) = operations(permissions(r), obj)
        operations*(r : R, obj : OBJ) = operations(permissions*(r), obj)
        operations(u : U, obj : OBJ) = operations(roles(u), obj)
        operations*(u : U, obj : OBJ) = operations*(roles(u), obj)

A permission is a pair consisting of an operation and an object. The
`objects` function is a mere selector or projection that extracts the
unique object from a permission and just returns it as a singleton
set. The `operations` function is different in that it takes an object
as second argument. If the first argument is a permission the result
is an empty set or a singleton set depending on the permission's
object. If the first argument is a role all operations from the role's
permissions that operate on the given object are returned. Note, how
simple the original `operations` function for roles can be expressed
using the added `operations` function for permissions. The
`operations*` functions additionally consider the permissions due to
junior roles of the input role. No `*` is allowed for the simple
`objects` function and the `operations` function applied to
permissions!

A session has a unique name, belongs to a unique user, and comprises a
set of so called *activated* roles. The overloaded function `user`
(where `*` would be illegal) returns the unique user of the session
given as input. (In case of need this single user is converted into a
singleton user set.) The inverse function `sessions` (where `*` is
also illegal) returns a proper set of sessions namely all sessions
belonging to the user given as input. This set may be empty, a
singleton set or a larger set, as a single user may *open* several
sessions.

        user(s) = defined unique user of session s
        sessions(u) = { s | user(s) = u }

The `roles` function applied to a session returns the *explicitely
activated* roles of this session. These *activated* roles must be a
subset of the roles *authorized* by the session's user. Simply
requiring `roles(s)` to be a subset of `roles(user(s))` for a session
`s` is not good enough in the presence of role hierarchies. Any role
from `roles*(user(s))` may be activated in session `s` and any role
from `roles*(s)` will be activated either explicitely or implicitely.
An RCL statement to express the required subset relation between
activated and authorized roles would the following, but this relation is
enforced internally:

        OE(roles(OE(S))) ∈ roles*(user(OE(S)))

In addition to the `juniors*` and `seniors*` functions also `juniors`
and `seniors` functions without `*` are provided. These functions
denote the *immediate* junior or senior roles of an input role. It is
the irreflexive transitive reduction of the role hierarchy. The
immediate subrole relation is denoted using `<<` and is a subrelation
of `<=`.

        juniors : R -> 2^R, juniors(r) = { j | j << r }
        seniors : R -> 2^R, seniors(r) = { s | r << s }

With these functions also *limited* role hierarchies could be
specified using RCL. The statement "`|juniors(OE(R))| <= 1`" would
restrict any role in the role hierarchy to have at most a single
immediate junior role. Such a hierarchy would be a forest of trees
with roots at the bottom. A reversed limited hierarchy with roots at
the top would result when using `seniors` for single immediate
seniors. In practise this may be rarely useful, though.

A statement "`|juniors(OE(R))| = 0`" or "`|juniors*(OE(R))| = 1`" or
equivalently for `seniors` would even prohibit a role hierarchy! This
demonstrates the expresive power of RCL with these additional
`juniors` and `seniors` functions.

The conflict sets `CR`, `CP`, or `CU` described in the [paper][1] are
set of sets of `R`, `P`, or `U` (with types `Rss`, `Pss`, or `Uss`),
respectively. These sets can be used to describe separation of duty
(SoD) constraints in RCL.

        |roles*(OE(U)) ∩ OE(CR)| ≤ 1

The authorized roles of any user `OE(U)` may at most contain a single
role from any conflict set `OE(CR)`. In the case of two conflicting
roles `r1` and `r2` and a singleton set of a conflict set
(`CR={{r1,r2}}`), no user `u=OE(U)` from `U` could be assigned (or
authorized) to both roles without violating the
constraint. `OE(CR)={r1,r2}` would be a subset of `roles*(u)` thus the
intersection would contain two elements rather than at most one.

For a role hierarchy the function `roles*` must be used in the above
statement. However, it does not make sense to put a junior and senior
role into one conflict set, because this would imply that only the
junior role could be authorized. If the senior role is authorized then
the junior role is authorized implicitely yielding the unintended
conflict. In fact only *incomparable* roles with respect to the
hierarchy relation `<=` should be put into conflict sets! Singleton
(or even empty) conflict sets as elements of `CR` also do not make
sense as for these sets the intersection will always be at most one
element.

The [paper][1] distinguishes *static* (SSoD) and *dynamic* (DSoD)
separation of duty. SSoD addresses *assigned* (or with a role
hierarchy *authorized*) roles whereas DSoD addresses the *activated*
roles of sessions as in the following statement.

        |roles*(sessions(OE(U))) ∩ OE(CR)| ≤ 1

It should be noted that the set of conflict sets `CR` can not be used
if static and dynamic SoD constraints should be enforced
*simultaneously*, because a static constraint already rules out the
mere activation of conflicting roles in sessions. For both, SSoD and
DSoD, two different sets of sets need to and *can be* defined,
i.e. `SCR` and `DCR`. `CR` (as well as `CP` or `CU`) are not builtin
but *user defined*. `SCR` and `DCR` can be defined in the
[types](examles/types.txt) file in type checking (`-t`) mode or in the
[sets](examles/sets.txt) file.

Usually several permissions are assigned to a single role. In order to
avoid conflicts maybe more intuitively, a set of conflicting
permission sets like `CP` may be used.

        |permissions(roles*(OE(U))) ∩ OE(CP)| ≤ 1

Here the use of `permissions*` is not required as `roles*` already
transitively closes the role sets. The same statment can be expressed
equivalently.

        |permissions*(roles(OE(U))) ∩ OE(CP)| ≤ 1

Using `*` for both functions would also not change the semantics. To
switch the static constraint into a dynamic one, again roles from
`sessions` need to be taken.

        |permissions(roles*(sessions(OE(U)))) ∩ OE(CP)| ≤ 1

The difference between permissions and roles is not that striking as
permission conflicts imply role conflicts. Again it is no good idea to
assign conflicting permissions to roles comparable via `<=`. Assigning
conflicting permissions to a single role or comparable roles can be
avoided by the following constraint.

         |permissions*(OE(R)) ∩ OE(CP)| ≤ 1

The set `CU` can be used to restrict badly collaborating users. The
interactions with conflicting roles and the presence of role
hierarchies may be difficult to grasp, though, and is therefore left
to the literature or as an exercise.

## Comparison with the [RBAC ANSI INCITS 359][3]

RBAC ANSI has a couple of review functions for queries that correspond
to the above RCL functions as follows.

    RC-11 AssignedUsers(r, result) <=> result = users(r)
    RC-09 AssignedRoles(u, result) <=> result = roles(u)
    RC-34 RolePermissions(r, result) <=> result = permissions*(r)
    RC-43 UserPermissions(u, result) <=> result = permissions*(u)
    RC-36 SessionRoles(s, result) <=> result = roles*(s)
    RC-35 SessionPermissions(s, result) <=> result = permissions*(s)
    RC-33 RoleOperationsOnObject(r, obj, result) <=> result = operations*(r, obj)
    RC-42 UserOperationsOnObject(u, obj, result) <=> result = operations*(u, obj)
    RC-12 AuthorizedUsers(r, result) <=> result = users*(r)
    RC-13 AuthorizedRoles(u, result) <=> result = roles*(u)

`SessionRoles` and `SessionPermissions` must consider a possible role
hierarchy. (This seems to be wrong in [ANSI INCITS 359][3].) The names
for `...Permissions` and `...OperationsOnObject` are used with and
without a role hierarchy whereas `...Users` and `...Roles` are
prefixed using `Assigned...` without and using `Authorized...` with a
role hierarchy.

The ANSI review functions complement administrative commands and
supporting system functions for creating and managing RBAC
elements. Currently, RBAC elements for RCL are initially read from
files as described [below](#usage).

[RBAC ANSI][3] also describes static and dynamic separation of duty
relations. A static SoD (SSD) addresses *authorized* roles of a user
(`AssignedRoles/AuthorizedRoles`) wheres a dynamic SoD (DSD) addresses
the *activated* roles of a session (`SessionRoles`). For any SoD a
named set of roles can be associated with a number between 2 and the
cardinality of the set. If this number of roles from the given set are
taken simultaneously the SoD constraint is violated. Given a number
(`n>=2`) and a set of roles (`cr`) with at least `n` elements, then
at most `n-1` roles of this set may be assigned/authorized (SSD) or
activated (DSD). Using RCL we can express this by:

        |roles*(OE(U)) ∩ cr| < n  // SSD
        |roles*(OE(S)) ∩ cr| < n  // DSD

Note that we use `roles*` also for sessions. (Therefore `SessionRoles`
must consider a role hierarchy, too.) Furthermore, the second DSD does
not prevent a user to take `n` (or more) roles via *several*
sessions. In order to prevent the latter the RCL constraint would need
to be:

        |roles*(sessions(OE(U))) ∩ cr| < n  // DSD

For different numbers `n` different set of set of roles could be
defined, like the above conflicting roles set `CR` for
`n=2`. Summerizing, RCL is also able or even better suited to express
typical static or dynamic SoD constraints.

## Usage

    find . -name rcl2000-exe
    # find the binary and put it into your PATH
    # or use "stack run --" or "stack exec rcl2000-exe --" instead of "rcl2000-exe"
    rcl2000-exe -o. Stmts.rcl
    use Stmts.use Stmts.soil


The above call relies on the files
[rh, ua, pa, pu, s, and sets](examples) from the [examples](examples)
directory. The default file extension is `.txt`, but could be changed
using the `-x` option.

[rh](examples/rh.txt) contains roles, usually one role on a
separate line. Several roles on one line define a senior role and all
(direct) junior roles of this senior role.

[ua](examples/ua.txt) introduces all users with there (statically)
assigned roles. This defines the user-to-role assignment or a single
user if there is only one name in a line.

[pa](examples/pa.txt) defines the permission-to-role
assignment. Each permission is given by two leading words of a line.
The first word is the operation and second the object of the
permission. The internal name of a permission will be simply the
operation joined with the object by an (additional) underscore.

[pu](examples/pu.txt) defines the history of users that executed
certain permissions. Each permission is given by two leading words of a line.
The first word is the operation and second the object of the
permission. All other words of the same line are users.

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

## Interactive mode

The `-i` option sends rcl2000 into interactive mode that can be
terminated by entering `q` or pressing `Ctrl-C` (interrupt) or
`Ctrl-D` (end of input). You may also try to enter `h` for help or
some set expressions. This feature is still provisional.

The `-h` option displays a usage message.

The commands, sets, or statements entered at the prompt are kept in a
history file named `.haskeline_history`. If a file `.haskeline`
containing `historyDuplicates: IgnoreAll` exists, then duplicate
entries are avoided. This functionality is provided by the haskell
package [haskeline](https://github.com/judah/haskeline).

## References

[RCL paper 2000][1]

[USE tool 2007][2]

[ANSI RBAC 2004][3]

[1]: https://dl.acm.org/doi/10.1145/382912.382913
[2]: https://sourceforge.net/projects/useocl
[3]: http://profsandhu.com/journals/tissec/ANSI+INCITS+359-2004.pdf
