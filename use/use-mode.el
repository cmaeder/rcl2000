(require 'generic-x) ;; we need this

(define-generic-mode 'use-mode      ;; name of the mode to create
  '("//" "--" ("/*" . "*/"))        ;; comments start with '//' or '--'
  '("abstract" "aggregation" "association" "associationclass" "attributes" "begin" "between" "class" "composition" "constraints" "context" "declare" "delete" "destroy" "do" "else" "end" "endif" "enum" "for" "from" "if" "in" "init" "insert" "into" "inv" "let" "model" "new" "operations" "ordered" "post" "pre" "psm" "role" "then" "statemachines" "states" "transitions")
                                    ;; some keywords
  '((":=" . 'font-lock-keyword-face);; ':=' is an operator
    ("\\.\\.?\\|::?\\|[][,;#{}()|]" . 'font-lock-builtin-face)
                                    ;; ';' is a built-in
    ("'[^']*'" . 'font-lock-string-face)
    ("/\\*\\([^*]\\|\\*+[^/]\\)*\\*+/" . 'font-lock-comment-face)
    ("\\<\\(Real\\|Integer\\|Boolean\\|Collection\\|String\\|\\(Ordered\\)?Set\\|Bag\\|Sequence\\)\\>" . 'font-lock-type-face)
    ("\\<\\(\\(in\\|ex\\)clud\\(es\\(All\\)?\\|ing\\)\\|is\\(Undefined\\|Unique\\|Defined\\)\\|ocl\\(As\\(Type\\|Set\\)\\|Is\\(Type\\|Kind\\)Of\\|Is\\(New\\|Undefined\\|Invalid\\)\\)\\|allInstances\\|collect\\(Nested\\)?\\|closure\\|abs\\|floor\\|round\\|max\\|min\\|div\\|mod\\|size\\|count\\|isEmpty\\|notEmpty\\|sum\\|union\\|intersection\\|iterate\\|flatten\\|as\\(Bag\\|Set\\|Sequence\\)\\|at\\|append\\|prepend\\|subSequence\\|first\\|last\\|select\\|reject\\|forAll\\|exists\\|and\\|or\\|not\\|implies\\|any\\|true\\|false\\|one\\)\\>\\|<>\\|>=?\\|<=?\\|->?\\|[=+*/]"
    . 'font-lock-function-name-face)
    ("\\<\\(result\\|self\\)\\>" . 'font-lock-variable-name-face))
  '("\\.use$")                      ;; files for which to activate this mode
  nil                               ;; other functions to call
  "A mode for .use files"           ;; doc string for this mode
  )
