;;  lambda-list
(&whole :constant ampersand :name whole)
(&optional :constant ampersand :name optional)
(&rest :constant ampersand :name rest)
(&body :constant ampersand :name body)
(&key :constant ampersand :name key)
(&allow-other-keys :constant ampersand :name allow)
(&aux :constant ampersand :name aux)
(&environment :constant ampersand :name environment)

;;  common-lisp special
*break-on-signals*
*compile-file-pathname*
*compile-file-truename*
*compile-print*
*compile-verbose*
*debug-io*
*debugger-hook*
*default-pathname-defaults*
*error-output*
*features*
*gensym-counter*
*load-pathname*
*load-print*
*load-truename*
*load-verbose*
*macroexpand-hook*
*modules*
*package*
*print-array*
*print-base*
*print-case*
*print-circle*
*print-escape*
*print-gensym*
*print-length*
*print-level*
*print-lines*
*print-miser-width*
*print-pprint-dispatch*
*print-pretty*
*print-radix*
*print-readably*
*print-right-margin*
*query-io*
*random-state*
*readtable*
*read-base*
*read-default-float-format*
*read-eval*
*read-suppress*
*standard-input*
*standard-output*
*terminal-io*
*trace-output*


;;  keyword
:abort
:absolute
:accessor
:adjustable
:after
:allocation
:allow-other-keys
:and
:append
:array
:argument-precedence-order
:arguments
:around
:base
:before
:block
:capitalize
:case
:class
:circle
:common
:common-lisp-user
:compile-toplevel
:conc-name
:constructor
:copier
:count
:create
:current
:datum
:declare
:default
:default-initargs
:defaults
:description
:device
:direction
:directory
:displaced-to
:displaced-index-offset
:documentation
:downcase
:element-type
:end
:end1
:end2
:environment
:error
:escape
:execute
:expected-type
:export
:external
:external-format
:fill
:fill-pointer
:from-end
:format-arguments
:format-control
:full
:generic-function
:generic-function-class
:gensym
:host
:identity
:identity-with-one-argument
:if-does-not-exist
:if-exists
:import-from
:include
:index
:initarg
:initform
:initial-element
:initial-contents
:initial-offset
:initial-value
:inherited
:input
:instance
:interactive
:interactive-function
:intern
:internal
:invert
:io
:junk-allowed
:key
:lambda-list
:length
:level
:linear
:line
:line-relative
:lines
:load-toplevel
:local
:mandatory
:metaclass
:method
:method-class
:method-combination
:miser
:miser-width
:most-specific-first
:most-specific-last
:name
:named
:new-version
:newest
:nicknames
:no-error
:not
:object
:operands
:operation
:operator
:or
:order
:output
:output-file
:override
:overwrite
:package
:pathname
:per-line-prefix
:pprint-dispatch
:predicate
:prefix
:preserve
:preserve-whitespace
:pretty
:print
:print-function
:print-object
:probe
:radix
:read-only
:readably
:reader
:rehash-size
:rehash-threshold
:relative
:rename
:rename-and-delete
:report
:report-function
:required
:right-margin
:section
:section-relative
:shadow
:shadowing-import-from
:size
:slot-names
:start
:start1
:start2
:stream
:suffix
:supersede
:test
:test-function
:test-not
:type
:unspecific
:up
:upcase
:use
:verbose
:version
:wild
:wild-inferiors
:writer


;;  &key syscall
:args
:cache
:exit
:if-error-exists
:if-input-does-not-exist
:if-output-exists
:pipe
:program
:search
:value
:wait


;;  common-lisp
(* :constant common :name asterisk)
(** :constant common :name asterisk2)
(*** :constant common :name asterisk3)
(+ :constant common :name plus)
(++ :constant common :name plus2)
(+++ :constant common :name plus3)
(- :constant common :name minus)
(/ :constant common :name slash)
(// :constant common :name slash2)
(/// :constant common :name slash3)
(1+ :constant common :name one-plus)
(1- :constant common :name one-minus)
(= :constant common :name number-equal)
(/= :constant common :name number-not-equal)
(< :constant common :name number-less)
(> :constant common :name number-greater)
(<= :constant common :name number-less-equal)
(>= :constant common :name number-greater-equal)
abort
abs
acons
acos
acosh
add-method
adjoin
adjust-array
adjustable-array-p
;; [mop]  allocate-instance
alpha-char-p
alphanumericp
and
atom
append
apply
apropos
apropos-list
arithmetic-error
arithmetic-error-operands
arithmetic-error-operation
aref
array
array-dimension
array-dimension-limit
array-dimensions
array-element-type
array-has-fill-pointer-p
array-displacement
array-in-bounds-p
array-rank
array-rank-limit
array-row-major-index
array-total-size
array-total-size-limit
arrayp
ash
asin
asinh
assert
assoc
assoc-if
assoc-if-not
atan
atanh
base-char
base-string
bignum
bit
bit-and
bit-andc1
bit-andc2
bit-eqv
bit-ior
bit-nand
bit-nor
bit-not
bit-orc1
bit-orc2
bit-vector
bit-vector-p
bit-xor
block
boole
boole-1
boole-2
boole-and
boole-andc1
boole-andc2
boole-c1
boole-c2
boole-clr
boole-eqv
boole-ior
boole-nand
boole-nor
boole-orc1
boole-orc2
boole-set
boole-xor
boolean
both-case-p
boundp
break
broadcast-stream
broadcast-stream-streams
built-in-class
butlast
byte
byte-size
byte-position
call-arguments-limit
call-method
call-next-method
car
cdr
caar
cadr
cdar
cddr
caaar
caadr
cadar
caddr
cdaar
cdadr
cddar
cdddr
caaaar
caaadr
caadar
caaddr
cadaar
cadadr
caddar
cadddr
cdaaar
cdaadr
cdadar
cdaddr
cddaar
cddadr
cdddar
cddddr
case
catch
ccase
ceiling
cell-error
cell-error-name
cerror
;; [mop]  change-class
char
character
characterp
(char= :constant common :name char-eql)
(char/= :constant common :name char-not-eql)
(char< :constant common :name char-less)
(char> :constant common :name char-greater)
(char<= :constant common :name char-less-equal)
(char>= :constant common :name char-greater-equal)
char-code
char-code-limit
char-downcase
char-equal
char-greaterp
char-int
char-lessp
char-name
char-not-equal
char-not-greaterp
char-not-lessp
char-upcase
check-type
cis
class
;; [mop]  class-name
class-of
clear-input
clear-output
close
clrhash
code-char
coerce
compilation-speed
compile
compiled-function
compiled-function-p
compiler-macro
compiler-macro-function
complement
complex
complexp
compute-applicable-methods
compute-restarts
concatenate
concatenated-stream
concatenated-stream-streams
cond
condition
conjugate
compile-file
compile-file-pathname
cons
consp
constantly
constantp
continue
control-error
copy-alist
copy-list
copy-pprint-dispatch
copy-readtable
copy-seq
copy-structure
copy-symbol
copy-tree
cos
cosh
count
count-if
count-if-not
ctypecase
debug
decf
declaration
declare
declaim
decode-float
decode-universal-time
defclass
defconstant
defgeneric
define-compiler-macro
define-condition
define-method-combination
define-modify-macro
define-setf-expander
define-symbol-macro
defmacro
defmethod
defpackage
defparameter
defsetf
defstruct
deftype
defun
defvar
delete
delete-duplicates
delete-file
delete-if
delete-if-not
delete-package
denominator
deposit-field
describe
describe-object
destructuring-bind
digit-char
digit-char-p
directory
directory-namestring
disassemble
division-by-zero
do
(do* :constant common :name doa)
documentation
dolist
dotimes
double-float
double-float-epsilon
double-float-negative-epsilon
do-all-symbols
do-external-symbols
do-symbols
dpb
dribble
dynamic-extent
ecase
echo-stream
echo-stream-input-stream
echo-stream-output-stream
ed
eighth
elt
encode-universal-time
end-of-file
endp
;; [mop]  ensure-generic-function
enough-namestring
ensure-directories-exist
eq
eql
equal
equalp
error
etypecase
eval
eval-when
evenp
every
exp
export
expt
extended-char
fboundp
fceiling
fdefinition
ffloor
fmakunbound
fifth
file-author
file-error
file-error-pathname
file-length
file-position
file-namestring
file-stream
file-string-length
file-write-date
fill
fill-pointer
find
find-all-symbols
find-class
find-if
find-if-not
find-method
find-package
find-restart
find-symbol
finish-output
first
fixnum
flet
float
floatp
floating-point-inexact
floating-point-invalid-operation
floating-point-overflow
floating-point-underflow
float-radix
float-sign
float-digits
float-precision
floor
force-output
format
formatter
fourth
fresh-line
fround
funcall
function
;; [mop]  function-keywords
function-lambda-expression
functionp
ftype
ftruncate
gcd
generic-function
gensym
gentemp
get
get-decoded-time
get-dispatch-macro-character
get-internal-real-time
get-internal-run-time
get-macro-character
get-output-stream-string
get-setf-expansion
get-universal-time
getf
gethash
get-properties
go
graphic-char-p
handler-bind
handler-case
hash-table
hash-table-count
hash-table-p
hash-table-rehash-size
hash-table-rehash-threshold
hash-table-size
hash-table-test
host-namestring
identity
if
ignorable
ignore
ignore-errors
imagpart
import
incf
;; [mop]  initialize-instance
inline
input-stream-p
inspect
integer
integerp
integer-decode-float
integer-length
interactive-stream-p
intern
internal-time-units-per-second
intersection
invalid-method-error
invoke-debugger
invoke-restart
invoke-restart-interactively
in-package
isqrt
keyword
keywordp
labels
lambda
lambda-list-keywords
lambda-parameters-limit
last
lcm
ldb
ldb-test
ldiff
least-negative-double-float
least-negative-long-float
least-negative-normalized-double-float
least-negative-normalized-long-float
least-negative-normalized-short-float
least-negative-normalized-single-float
least-negative-short-float
least-negative-single-float
least-positive-double-float
least-positive-long-float
least-positive-normalized-double-float
least-positive-normalized-long-float
least-positive-normalized-short-float
least-positive-normalized-single-float
least-positive-short-float
least-positive-single-float
length
let
(let* :constant common :name leta)
lisp-implementation-type
lisp-implementation-version
list
listen
listp
(list* :constant common :name lista)
list-all-packages
list-length
load
load-logical-pathname-translations
load-time-value
locally
log
logand
logandc1
logandc2
logbitp
logcount
logeqv
logical-pathname
logical-pathname-translations
logior
lognand
lognor
lognot
logorc1
logorc2
logtest
logxor
long-float
long-float-epsilon
long-float-negative-epsilon
long-site-name
loop
loop-finish
lower-case-p
machine-instance
machine-type
machine-version
macro-function
macroexpand
macroexpand-1
macrolet
make-array
make-broadcast-stream
make-condition
make-concatenated-stream
make-dispatch-macro-character
make-echo-stream
make-hash-table
;; [mop]  make-instance
make-instances-obsolete
make-list
make-load-form
make-load-form-saving-slots
make-method
make-package
make-pathname
make-random-state
make-sequence
make-string
make-string-input-stream
make-string-output-stream
make-symbol
make-synonym-stream
make-two-way-stream
makunbound
map
mapc
mapcar
mapcan
maphash
mapl
maplist
mapcon
map-into
mask-field
max
merge
method
method-combination-error
;; [mop]  method-qualifiers
member
member-if
member-if-not
merge-pathnames
method-combination
min
minusp
mismatch
mod
most-negative-double-float
most-negative-fixnum
most-negative-long-float
most-negative-short-float
most-negative-single-float
most-positive-double-float
most-positive-fixnum
most-positive-long-float
most-positive-short-float
most-positive-single-float
muffle-warning
multiple-value-bind
multiple-value-call
multiple-value-list
multiple-value-prog1
multiple-value-setq
multiple-values-limit
name-char
namestring
nbutlast
nconc
next-method-p
;;nil
ninth
nintersection
no-applicable-method
no-next-method
not
notany
notevery
notinline
nreconc
nreverse
nset-difference
nset-exclusive-or
nstring-capitalize
nstring-downcase
nstring-upcase
nsublis
nsubst
nsubstitute
nsubstitute-if
nsubstitute-if-not
nsubst-if
nsubst-if-not
nth
nth-value
nthcdr
null
number
numberp
numerator
nunion
oddp
otherwise
open
open-stream-p
optimize
or
output-stream-p
package
packagep
package-error
package-error-package
package-name
package-nicknames
package-shadowing-symbols
package-use-list
package-used-by-list
pairlis
parse-error
parse-integer
parse-namestring
pathname
pathname-host
pathname-device
pathname-directory
pathname-name
pathname-match-p
pathname-type
pathname-version
pathnamep
peek-char
phase
pi
plusp
pop
position
position-if
position-if-not
pprint
pprint-dispatch
pprint-exit-if-list-exhausted
pprint-fill
pprint-indent
pprint-linear
pprint-logical-block
pprint-newline
pprint-pop
pprint-tab
pprint-tabular
prin1
prin1-to-string
princ
princ-to-string
print
print-not-readable
print-not-readable-object
print-object
print-unreadable-object
probe-file
proclaim
prog
prog1
prog2
(prog* :constant common :name proga)
progn
progv
program-error
provide
psetf
psetq
push
pushnew
quote
random
random-state
random-state-p
rassoc
rassoc-if
rassoc-if-not
ratio
rational
rationalize
rationalp
read
read-byte
read-char
read-char-no-hang
read-delimited-list
read-from-string
read-line
read-preserving-whitespace
read-sequence
reader-error
readtable
readtable-case
readtablep
real
realp
realpart
reduce
;; [mop]  reinitialize-instance
rem
remf
remhash
remove
remove-duplicates
remove-if
remove-if-not
remove-method
remprop
rename-file
rename-package
replace
require
rest
restart
restart-bind
restart-case
restart-name
return
return-from
revappend
reverse
room
rotatef
round
row-major-aref
rplaca
rplacd
safety
satisfies
sbit
scale-float
schar
search
second
sequence
serious-condition
set
set-dispatch-macro-character
set-macro-character
set-pprint-dispatch
set-syntax-from-char
setf
setq
set-difference
set-exclusive-or
seventh
shadow
shadowing-import
;; [mop]  shared-initialize
shiftf
short-float
short-float-epsilon
short-float-negative-epsilon
short-site-name
signal
signed-byte
signum
simple-array
simple-base-string
simple-bit-vector
simple-bit-vector-p
simple-condition
simple-condition-format-control
simple-condition-format-arguments
simple-error
simple-string
simple-string-p
simple-type-error
simple-vector
simple-vector-p
simple-warning
sin
sinh
single-float
single-float-epsilon
single-float-negative-epsilon
sixth
sleep
slot-boundp
slot-exists-p
slot-makunbound
;; [mop]  slot-missing
;; [mop]  slot-unbound
slot-value
software-type
software-version
some
sort
space
special
special-operator-p
speed
sqrt
stable-sort
standard
standard-char
standard-char-p
standard-class
standard-generic-function
standard-method
standard-object
step
storage-condition
store-value
stream
streamp
stream-element-type
stream-error
stream-error-stream
stream-external-format
string
(string= :constant common :name string-eql)
(string/= :constant common :name string-not-eql)
(string< :constant common :name string-less)
(string> :constant common :name string-greater)
(string<= :constant common :name string-less-equal)
(string>= :constant common :name string-greater-equal)
string-capitalize
string-downcase
string-equal
string-greaterp
string-left-trim
string-lessp
string-not-equal
string-not-greaterp
string-not-lessp
string-right-trim
string-stream
string-trim
string-upcase
stringp
structure
structure-class
structure-object
style-warning
subsetp
subseq
sublis
subst
substitute
substitute-if
substitute-if-not
subst-if
subst-if-not
subtypep
svref
sxhash
symbol
symbol-function
symbol-macrolet
symbol-name
symbol-package
symbol-plist
symbol-value
symbolp
synonym-stream
synonym-stream-symbol
tagbody
tailp
tan
tanh
tenth
terpri
the
third
throw
time
trace
translate-logical-pathname
translate-pathname
tree-equal
truename
truncate
two-way-stream
two-way-stream-input-stream
two-way-stream-output-stream
type
typep
type-error
type-error-datum
type-error-expected-type
type-of
typecase
unbound-slot
unbound-slot-instance
unbound-variable
undefined-function
unexport
unintern
union
unless
unread-char
unsigned-byte
untrace
unuse-package
unwind-protect
update-instance-for-different-class
update-instance-for-redefined-class
upgraded-array-element-type
upgraded-complex-part-type
upper-case-p
use-package
use-value
user-homedir-pathname
values
values-list
variable
vector
vector-pop
vector-push
vector-push-extend
vectorp
warn
warning
wild-pathname-p
with-accessors
with-compilation-unit
with-condition-restarts
with-hash-table-iterator
with-input-from-string
with-open-file
with-open-stream
with-output-to-string
with-package-iterator
with-simple-restart
with-slots
with-standard-io-syntax
when
write
write-byte
write-char
write-line
write-sequence
write-string
write-to-string
yes-or-no-p
y-or-n-p
zerop

;;  common-lisp package
lisp-clos::datum
lisp-clos::expected-type
lisp-clos::format-arguments
lisp-clos::format-control
lisp-clos::operands
lisp-clos::operation
lisp-clos::package
lisp-clos::pathname
lisp-clos::stream

;;  clos keyword
lisp-clos::name
lisp-clos::direct-slots
lisp-clos::direct-subclasses
lisp-clos::direct-superclasses
lisp-clos::class-precedence-list
lisp-clos::effective-slots
lisp-clos::finalized-p
lisp-clos::prototype
lisp-clos::default-initargs
lisp-clos::direct-default-initargs
lisp-clos::version
lisp-clos::redefined-class
(:name                       :constant closkey)
(:direct-slots               :constant closkey)
(:direct-subclasses          :constant closkey)
(:direct-superclasses        :constant closkey)
(:class-precedence-list      :constant closkey)
(:effective-slots            :constant closkey)
(:finalized-p                :constant closkey)
(:prototype                  :constant closkey)
(:default-initargs           :constant closkey)
(:direct-default-initargs    :constant closkey)
(:version                    :constant closkey)
(:redefined-class            :constant closkey)

lisp-clos::methods
lisp-clos::lambda-list
lisp-clos::argument-precedence-order
lisp-clos::declarations
lisp-clos::method-class
lisp-clos::method-combination
lisp-clos::vector
lisp-clos::remove
lisp-clos::argument
lisp-clos::eqlcheck
lisp-clos::cache
lisp-clos::call
lisp-clos::function
lisp-clos::precedence-index
(:methods                    :constant closkey)
(:lambda-list                :constant closkey)
(:argument-precedence-order  :constant closkey)
(:declarations               :constant closkey)
(:method-class               :constant closkey)
(:method-combination         :constant closkey)
(:vector                     :constant closkey)
(:remove                     :constant closkey)
(:argument                   :constant closkey)
(:eqlcheck                   :constant closkey)
(:cache                      :constant closkey)
(:call                       :constant closkey)
(:function                   :constant closkey)
(:precedence-index           :constant closkey)

lisp-clos::generic-function
lisp-clos::qualifiers
lisp-clos::specializers
lisp-clos::long-p
lisp-clos::documentation
lisp-clos::identity
lisp-clos::operator
lisp-clos::arguments
lisp-clos::generic
lisp-clos::form
lisp-clos::declare
(:generic-function           :constant closkey)
(:qualifiers                 :constant closkey)
(:specializers               :constant closkey)
(:long-p                     :constant closkey)
(:documentation              :constant closkey)
(:identity                   :constant closkey)
(:operator                   :constant closkey)
(:arguments                  :constant closkey)
(:generic                    :constant closkey)
(:form                       :constant closkey)
(:declare                    :constant closkey)

lisp-clos::object
lisp-clos::type
(:object                     :constant closkey)
(:type                       :constant closkey)

lisp-clos::readers
lisp-clos::writers
lisp-clos::accessors
lisp-clos::initargs
lisp-clos::initform
lisp-clos::initfunction
lisp-clos::allocation
lisp-clos::instance
lisp-clos::class
lisp-clos::metaclass
lisp-clos::binding
lisp-clos::order
(:readers                    :constant closkey)
(:writers                    :constant closkey)
(:accessors                  :constant closkey)
(:initargs                   :constant closkey)
(:initform                   :constant closkey)
(:initfunction               :constant closkey)
(:allocation                 :constant closkey)
(:instance                   :constant closkey)
(:class                      :constant closkey)
(:metaclass                  :constant closkey)
(:binding                    :constant closkey)
(:order                      :constant closkey)

;;  structure
lisp-clos::slots
lisp-clos::include
lisp-clos::value
lisp-clos::predicate
lisp-clos::access
lisp-clos::copier
lisp-clos::constructor
(:slots                      :constant closkey)
(:include                    :constant closkey)
(:value                      :constant closkey)
(:predicate                  :constant closkey)
(:access                     :constant closkey)
(:copier                     :constant closkey)
(:constructor                :constant closkey)

;;  system
lisp-system::value ;; symbol, function
lisp-system::function ;; symbol, function
lisp-system::setf ;; symbol, function
lisp-system::inline-function ;; symbol
lisp-system::inline-setf ;; symbol
lisp-system::tagbody
lisp-system::block
lisp-system::declaim
lisp-system::defun
lisp-system::defmacro
lisp-system::deftype
lisp-system::define-compiler-macro
lisp-system::macro-lambda
lisp-system::destructuring-bind
lisp-system::special
lisp-system::lexical
lisp-system::local
lisp-system::global
lisp-system::lambda
lisp-system::scope
lisp-system::multiple-value-bind

lisp-system::declaration
lisp-system::inline
lisp-system::dynamic-value
lisp-system::dynamic-function
lisp-system::ignore-value
lisp-system::ignore-function

lisp-system::type
lisp-system::type-special
lisp-system::type-lexical
lisp-system::type-value
lisp-system::type-function
lisp-system::type-setf

lisp-system::function-argtype
lisp-system::function-rettype

;; handler-bind/case, restart-bind/case
lisp-system::handler
lisp-system::handler-bind
lisp-system::handler-case
lisp-system::restart
lisp-system::restart-bind
lisp-system::restart-case
(lisp-system::*enable-debugger* :name enable-debugger :constant system)
(lisp-system::*index-debugger* :name index-debugger :constant system)

(lisp-system::*parse-environment* :name eval-parse-environment :constant system)
(lisp-system::*parse-declare* :name parse-declare :constant system)
(lisp-system::*scope* :name eval-scope :constant system)
(lisp-system::*scope-global* :name eval-scope-global :constant system)

;; eval-when
(lisp-system::*eval-toplevel* :name eval-toplevel :constant system)
(lisp-system::*eval-compile-time* :name eval-compile-time :constant system)
(lisp-system::*eval-compile-toplevel* :name eval-compile-toplevel :constant system)
(lisp-system::*eval-load-toplevel* :name eval-load-toplevel :constant system)
(lisp-system::*eval-execute* :name eval-execute :constant system)

lisp-system::standard
(lisp-system::*environment* :name special-environment :constant system)
(lisp-system::*arguments* :name special-arguments :constant system)

(lisp-system::*load-size* :name load-size :constant system)
(lisp-system::*load-array* :name load-array :constant system)
(lisp-system::*load-table* :name load-table :constant system)
(lisp-system::*load-gensym* :name load-gensym :constant system)
(lisp-system::*load-depend* :name load-depend :constant system)
(lisp-system::*load-push* :name load-push :constant system)

(lisp-system::*depend-loop* :name depend-loop :constant system)
(lisp-system::*depend-pass* :name depend-pass :constant system)
(lisp-system::*depend-error* :name depend-error :constant system)
(lisp-system::*depend-root* :name depend-root :constant system)

lisp-system::compiled-macro-function
lisp-system::control
lisp-system::code
lisp-system::callname
lisp-system::eval
lisp-system::index
lisp-system::system
lisp-system::quote
lisp-system::environment
lisp-system::character2
lisp-system::charqueue
lisp-system::charbit
lisp-system::symstack
lisp-system::bittype
lisp-system::readlabel
(("LISP-SYSTEM" "READINFO") :constant system :name readinfo-symbol)
lisp-system::readtype
lisp-system::bitcons
lisp-system::bitbuffer
lisp-system::hashiterator
lisp-system::packageiterator
lisp-system::taginfo
lisp-system::array-dimension
lisp-system::array-general
lisp-system::array-specialized
lisp-system::unbound
lisp-system::space
lisp-system::space1
lisp-system::reserved
lisp-system::end
lisp-system::prompt-stream
lisp-system::pretty-stream
lisp-system::memory-stream
lisp-system::pipe-stream
(lisp-system::*gchold* :name gchold :constant system)


;;
;;  condition
;;
lisp-system::simple-control-error
lisp-system::simple-file-error
lisp-system::simple-package-error
lisp-system::simple-parse-error
lisp-system::simple-program-error
lisp-system::simple-reader-error
lisp-system::simple-style-warning


;;
;;  iteration
;;
lisp-system::loop-initially
lisp-system::loop-finally
lisp-system::loop-with
lisp-system::loop-for-as
lisp-system::loop-for-as-in-list
lisp-system::loop-for-as-on-list
lisp-system::loop-for-as-equals-then
lisp-system::loop-for-as-across
lisp-system::loop-for-as-hash
lisp-system::loop-for-as-package-symbol
lisp-system::loop-for-as-package-present
lisp-system::loop-for-as-package-external
lisp-system::loop-for-as-arithmetic-up
lisp-system::loop-for-as-arithmetic-downto
lisp-system::loop-for-as-arithmetic-downfrom
lisp-system::loop-do
lisp-system::loop-return
lisp-system::loop-if
lisp-system::loop-unless
lisp-system::loop-collect
lisp-system::loop-append
lisp-system::loop-nconc
lisp-system::loop-count
lisp-system::loop-sum
lisp-system::loop-maximize
lisp-system::loop-minimize
lisp-system::loop-while
lisp-system::loop-until
lisp-system::loop-repeat
lisp-system::loop-always
lisp-system::loop-never
lisp-system::loop-thereis

(lisp-system::*loop-named* :name loop-named :constant system)
(lisp-system::*loop-vars* :name loop-vars :constant system)
(lisp-system::*loop-init* :name loop-init :constant system)
(lisp-system::*loop-final* :name loop-final :constant system)
(lisp-system::*loop-form* :name loop-form :constant system)
(lisp-system::*loop-let* :name loop-let :constant system)

lisp-system::next-loop
lisp-system::end-loop
lisp-system::value-loop
lisp-system::function-loop
lisp-system::it-loop


;;
;;  structure
;;
lisp-system::structure-gensym
lisp-system::structure-named


;;
;;  hash-table
;;
lisp-system::cache


;;
;;
;;  symbol
;;
lisp-system::type-documentation


;;
;;  eval
;;
lisp-system::compiler-macro-function
lisp-system::setf-compiler-macro-function
(lisp-system::*enable-compiler-macro* :constant system :name enable-compiler-macro)
lisp-system::compile-warning
lisp-system::compile-style-warning
lisp-system::nth-value
(lisp-system::optimize-check :export t)


;;
;;  number
;;
lisp-system::cast-single-float
lisp-system::cast-double-float
lisp-system::cast-long-float


;;
;;  type
;;
lisp-system::bytespec
lisp-system::type-symbol
lisp-system::type-list


;;
;;  pathname
;;
lisp-system::unix
lisp-system::windows
lisp-system::universal
lisp-system::device
lisp-system::logical-pathname
lisp-system::time1970
(lisp-system::*load-logical-pathname-translations*
  :constant system :name load-logical-pathname-translations)


;;
;;  file stream
;;
(lisp-system::*external-format* :constant system :name external-format)
lisp-system::ascii
lisp-system::utf-8
lisp-system::utf-8-bom
lisp-system::utf-16
lisp-system::utf-16le
lisp-system::utf-16be
lisp-system::utf-16le-bom
lisp-system::utf-16be-bom
lisp-system::utf-32
lisp-system::utf-32le
lisp-system::utf-32be
lisp-system::utf-32le-bom
lisp-system::utf-32be-bom
(lisp-system::*end-of-line* :constant system :name end-of-line)
lisp-system::cr
lisp-system::lf
lisp-system::crlf
lisp-system::auto
lisp-system::close-abort


;;
;;  printer
;;
lisp-system::print-dispatch
(lisp-system::*print-write* :constant system :name print-write)
lisp-system::n
lisp-system::a
lisp-system::h
lisp-system::w
lisp-system::f
lisp-system::na
(lisp-system::*default-print-dispatch*
  :constant system :name default-print-dispatch)
(lisp-system::*empty-print-dispatch*
  :constant system :name empty-print-dispatch)
lisp-system::dispatch-vector
lisp-system::dispatch-quote
lisp-system::dispatch-call
lisp-system::dispatch-defun
lisp-system::dispatch-let


;;
;;  system
;;
lisp-system::delay-warning
(lisp-system::*delay-warning-list* :constant system :name delay-warning-list)
(lisp-system::*delay-warning-switch* :constant system :name delay-warning-switch)
(lisp-system::*module-provider-functions*
  :constant system :name module-provider-functions)
(lisp-system::*compile-output* :constant system :name compile-output)
(lisp-system::*compile-code* :constant system :name compile-code)


;;
;;  environment
;;
(lisp-system::*encode-universal-1970* :constant system :name encode-universal-1970)
(lisp-system::*ed-function* :constant system :name ed-function)
(lisp-system::*ed-tempfile* :constant system :name ed-tempfile)
(lisp-system::*ed-program* :constant system :name ed-program)
lisp-system::doc-type
lisp-system::object
(lisp-system::*trace-list* :constant system :name trace-list)
(lisp-system::*trace-depth* :constant system :name trace-depth)
(lisp-system::*dribble-file* :constant system :name dribble-file)
(lisp-system::*dribble-input* :constant system :name dribble-input)
(lisp-system::*dribble-output* :constant system :name dribble-output)
(lisp-system::*dribble-echo* :constant system :name dribble-echo)
(lisp-system::*dribble-broadcast* :constant system :name dribble-broadcast)
(lisp-system::*inspected* :constant system :name inspected)

(lisp-system::*step-begin* :constant system :name step-begin)
lisp-system::step


;;
;;  code
;;
lisp-code::nop
lisp-code::begin
lisp-code::begin-call
lisp-code::end
lisp-code::escape
lisp-code::escape-not
lisp-code::save
lisp-code::restore
lisp-code::normal
lisp-code::revert
lisp-code::revert-goto

lisp-code::set
lisp-code::push
lisp-code::push-result
lisp-code::push-values
lisp-code::nil-set
lisp-code::nil-push
lisp-code::t-set
lisp-code::t-push

lisp-code::lexical
lisp-code::lexical-set
lisp-code::lexical-push
lisp-code::lexical-rem
lisp-code::lexical-free
lisp-code::special
lisp-code::special-set
lisp-code::special-push
lisp-code::special-rem

lisp-code::declaim-special
lisp-code::declaim-type-value
lisp-code::declaim-type-function
lisp-code::declaim-inline
lisp-code::declaim-notinline
lisp-code::declaim-compilation
lisp-code::declaim-debug
lisp-code::declaim-safety
lisp-code::declaim-space
lisp-code::declaim-speed
lisp-code::declaim-declaration

lisp-code::type-result
lisp-code::type-lexical
lisp-code::type-special
lisp-code::type-global
lisp-code::type-function
lisp-code::type-setf
lisp-code::let-lexical
lisp-code::let-special
(lisp-code::let*-special :constant code :name leta-special)

lisp-code::setq-lexical
lisp-code::setq-special
lisp-code::setq-global

lisp-code::function-set
lisp-code::function-push
lisp-code::setf-set
lisp-code::setf-push

lisp-code::defmacro
lisp-code::deftype
lisp-code::define-compiler-macro
lisp-code::defun

lisp-code::call-name
lisp-code::call-result
lisp-code::call-type
lisp-code::call-key
lisp-code::call-function
lisp-code::call-setf
lisp-code::call-lexical

lisp-code::values-nil
lisp-code::values-set
lisp-code::the-set
lisp-code::the-push

lisp-code::if-unbound
lisp-code::if-nil
lisp-code::if-t
lisp-code::goto
lisp-code::go
lisp-code::return-from
lisp-code::catch
lisp-code::throw
lisp-code::tag
lisp-code::taginfo
lisp-code::blockinfo

lisp-code::handler-bind
lisp-code::handler-case
lisp-code::restart-bind
lisp-code::restart-case
lisp-code::restart-progn

lisp-code::funcall
lisp-code::nth-value
lisp-code::progv

lisp-code::pop
lisp-code::pop-unbound
lisp-code::getf
lisp-code::rest_copy
lisp-code::rest_bind
lisp-code::allow-other-keys
lisp-code::rest-null
lisp-code::whole

lisp-code::lambda
lisp-code::lambda-name
lisp-code::lambda-type
lisp-code::lambda-doc
lisp-code::lambda-form
lisp-code::lambda-defun
lisp-code::lambda-closure
lisp-code::lambda-lexical
lisp-code::lambda-cache
lisp-code::lambda-cache-set
lisp-code::macro
lisp-code::macro-special
lisp-code::macro-env
lisp-code::macro-whole

lisp-code::labels-make
lisp-code::labels-lambda

lisp-code::bind1-type
lisp-code::bind1-special
lisp-code::bind1-lexical
lisp-code::bind2-type
lisp-code::bind2-special
lisp-code::bind2-lexical

lisp-code::load-alloc
lisp-code::load-gensym
lisp-code::load-set
lisp-code::reference-set
lisp-code::reference-push

lisp-code::step
lisp-code::step-off
lisp-code::step-begin
lisp-code::step-end


;;
;;  optimize
;;
lisp-code::optcode-result-type
lisp-code::optcode-car0-set
lisp-code::optcode-car0-push
lisp-code::optcode-car1-set
lisp-code::optcode-car1-push
lisp-code::optcode-cdr0-set
lisp-code::optcode-cdr0-push
lisp-code::optcode-cdr1-set
lisp-code::optcode-cdr1-push
lisp-code::optcode-cons


;;
;;  clos
;;
lisp-clos::metaobject
lisp-clos::specializer
lisp-clos::eql-specializer
lisp-clos::forward-referenced-class
lisp-clos::funcallable-standard-object
lisp-clos::funcallable-standard-class
lisp-clos::standard-reader-method
lisp-clos::standard-writer-method
lisp-clos::standard-accessor-method
lisp-clos::slot-definition
lisp-clos::direct-slot-definition
lisp-clos::effective-slot-definition
lisp-clos::standard-slot-definition
lisp-clos::standard-direct-slot-definition
lisp-clos::standard-effective-slot-definition


;;
;;  metaobject protocol
;;
(lisp-clos::allocate-instance :constant common)
(lisp-clos::initialize-instance :constant common)
(lisp-clos::reinitialize-instance :constant common)
(lisp-clos::shared-initialize :constant common)
(lisp-clos::ensure-generic-function :constant common)
(lisp-clos::make-instance :constant common)
(lisp-clos::slot-missing :constant common)
(lisp-clos::slot-unbound :constant common)
(lisp-clos::change-class :constant common)
(lisp-clos::function-keywords :constant common)

lisp-clos::redefined
lisp-clos::referenced-class
(lisp-clos::ensure-class :export t)
(lisp-clos::ensure-class-using-class :export t)
(lisp-clos::ensure-generic-function-using-class :export t)
(lisp-clos::ensure-method :export t)
(lisp-clos::slot-boundp-using-class :export t)
(lisp-clos::slot-exists-p-using-class :export t)
(lisp-clos::slot-makunbound-using-class :export t)
(lisp-clos::slot-value-using-class :export t)

lisp-clos::flet-method-p
lisp-clos::flet-next-method

lisp-clos::define-method-combination
lisp-clos::define-long-method-combination
lisp-clos::define-short-method-combination
lisp-clos::long-method-combination
lisp-clos::short-method-combination
lisp-clos::method-combination-instance
lisp-clos::ensure-method-combination-short
lisp-clos::ensure-method-combination-long
lisp-clos::qualifiers-elt
lisp-clos::combination-binding
lisp-clos::macro-make-method
lisp-clos::macro-call-method
lisp-clos::macro-method-lambda


;;
;;  mop-reader
;;
(lisp-clos::class-name :constant common)
lisp-clos::class-slots
lisp-clos::class-direct-slots
lisp-clos::class-default-initargs
lisp-clos::class-direct-default-initargs
;lisp-clos::class-precedence-list
lisp-clos::class-direct-superclasses
lisp-clos::class-direct-subclasses
lisp-clos::class-finalized-p
lisp-clos::class-prototype

lisp-clos::slot-definition-name
lisp-clos::slot-definition-type
lisp-clos::slot-definition-allocation
lisp-clos::slot-definition-initargs
lisp-clos::slot-definition-initform
lisp-clos::slot-definition-initfunction

lisp-clos::generic-function-name
lisp-clos::generic-function-methods
lisp-clos::generic-function-lambda-list
lisp-clos::generic-function-argument-precedence-order
lisp-clos::generic-function-declarations
lisp-clos::generic-function-method-class
lisp-clos::generic-function-method-combination

lisp-clos::method-function
lisp-clos::method-generic-function
lisp-clos::method-lambda-list
lisp-clos::method-specializers
(lisp-clos::method-qualifiers :constant common)
lisp-clos::accessor-method-slot-definition

lisp-clos::make-method-lambda
(lisp-clos::find-method-combination :export t)


;;
;;  rt
;;
(lisp-rt::*result* :constant rt :name result)
(lisp-rt::*index* :constant rt :name index)
(lisp-rt::*entries* :constant rt :name entries)
(lisp-rt::*entries-table* :constant rt :name entries-table)
(lisp-rt::*entries-warning* :constant rt :name entries-warning)
lisp-rt::error  ;; for deftest-error
lisp-rt::push-entries
lisp-rt::deftest
lisp-rt::deftest-error
(lisp-rt::deftest-error! :constant rt :name deftest-error-)
lisp-rt::do-tests
lisp-rt::rem-all-tests
lisp-rt::equalrt


;;
;;  readtable
;;
(("LISP-SYSTEM" "*READINFO*") :constant system :name readinfo-special)
(lisp-system::*eval-loop-exit* :constant system :name eval-loop-exit :export t)
(lisp-system::*prompt* :constant system :name prompt)
(lisp-system::*prompt-reading* :constant system :name prompt-reading)
(lisp-system::*prompt-value* :constant system :name prompt-value)
(lisp-system::*prompt-bright* :constant system :name prompt-bright :export t)
(lisp-system::*prompt-color* :constant system :name prompt-color :export t)
(lisp-system::*terme* :constant system :name special-terme)


;;
;;  restart name
;;
lisp-system::input


;;
;;  syscall object
;;
lisp-system::process


;;
;;  syscall
;;
lisp-system::readtable-dot
lisp-system::double-quote-reader
lisp-system::single-quote-reader
lisp-system::parensis-open-reader
lisp-system::parensis-close-reader
lisp-system::semicolon-reader
lisp-system::backquote-reader
lisp-system::comma-reader
lisp-system::sharp-reader
lisp-system::dispatch-function

lisp-system::error-dispatch
lisp-system::equal-dispatch
lisp-system::sharp-dispatch
lisp-system::single-quote-dispatch
lisp-system::parensis-open-dispatch
lisp-system::parensis-close-dispatch
lisp-system::asterisk-dispatch
lisp-system::colon-dispatch
lisp-system::less-dispatch
lisp-system::backslash-dispatch
lisp-system::or-dispatch
lisp-system::plus-dispatch
lisp-system::minus-dispatch
lisp-system::dot-dispatch
lisp-system::array-dispatch
lisp-system::binary-dispatch
lisp-system::complex-dispatch
lisp-system::octal-dispatch
lisp-system::pathname-dispatch
lisp-system::radix-dispatch
lisp-system::structure-dispatch
lisp-system::hexadecimal-dispatch

lisp-system::backquote
lisp-system::unbound-value

;;  subtypep!
lisp-system::include
lisp-system::exclude
lisp-system::invalid
lisp-system::false

lisp-system::subtypep-atomic
lisp-system::subtypep-atomic-not
lisp-system::subtypep-compound
lisp-system::subtypep-force-number
lisp-system::subtypep-normal

;;  terme
(lisp-system::terme-input :export t)
(lisp-system::terme-output :export t)
(lisp-system::terme-move :export t)
(lisp-system::terme-clear :export t)
(lisp-system::terme-delete :export t)
(lisp-system::terme-font :export t)
(lisp-system::terme-size :export t)
(lisp-system::terme-scroll :export t)
(lisp-system::terme-begin :export t)
(lisp-system::terme-end :export t)
(lisp-system::terme-enable :export t)

(lisp-system::terme-signal :export t)
(lisp-system::terme-hang :export t)
(lisp-system::terme-code :export t)
(lisp-system::terme-up :export t)
(lisp-system::terme-down :export t)
(lisp-system::terme-left :export t)
(lisp-system::terme-right :export t)
(lisp-system::terme-page-up :export t)
(lisp-system::terme-page-down :export t)
(lisp-system::terme-home :export t)
(lisp-system::terme-insert :export t)
(lisp-system::terme-function :export t)
(lisp-system::terme-escape :export t)

;;  fpclassify
lisp-system::fp-nan
lisp-system::fp-infinite
lisp-system::fp-normal
lisp-system::fp-subnormal
lisp-system::fp-zero

;;  paper
lisp-system::paper

;;  dlfile
(lisp-system::*dlfile* :constant system :name special-dlfile)

;; syscall_common.c
 lisp-system::define-symbol-macro
 lisp-system::symbol-macro-expander
 lisp-system::defconstant
 lisp-system::in-package
 lisp-system::setplist
 lisp-system::remplist
 lisp-system::make-hash-iterator
 lisp-system::next-hash-iterator
 lisp-system::make-package-iterator
 lisp-system::next-package-iterator
 lisp-system::defpackage
 lisp-system::do-symbols
 lisp-system::do-external-symbols
 lisp-system::do-all-symbols
 lisp-system::getdoc-variable
 lisp-system::setdoc-variable
 lisp-system::ecase-error
 lisp-system::etypecase-error
 lisp-system::define-setf-expander
 lisp-system::end-input-stream
 lisp-system::make-extend-output-stream
(lisp-system::prompt-for :export t)
 lisp-system::print-unreadable-call
 lisp-system::write-default
 lisp-system::symbol-deftype
 lisp-system::delete-deftype
 lisp-system::ensure-structure
 lisp-system::structure-constructor
 lisp-system::loop-bind
 lisp-system::make-pprint-stream
 lisp-system::pprint-gensym
 lisp-system::pprint-exit
 lisp-system::pprint-pop
 lisp-system::pprint-check
 lisp-system::pprint-close
 lisp-system::pprint-pretty
 lisp-system::timeinfo
 lisp-system::trace-add
 lisp-system::trace-del
 lisp-system::with-compilation-unit
 lisp-system::set-slots
 lisp-system::intern-eql-specializer
 lisp-system::defgeneric-define
 lisp-system::defgeneric-method
 lisp-system::condition-restarts-push
 lisp-system::condition-restarts-pop
 lisp-system::condition-restarts-make
 lisp-system::make-restart
 lisp-system::restart-progn

;; syscall_function.c
(lisp-system::abort-lisp :export t)
(lisp-system::hello :export t)
(lisp-system::infobit :export t)
(lisp-system::infoprint :export t)
(lisp-system::gc :export t)
(lisp-system::savecore :export t)
(lisp-system::loadcore :export t)
(lisp-system::*core-input* :constant system :name core-input)
(lisp-system::*core-output* :constant system :name core-output)
(lisp-system::package-export-list :export t)
(lisp-system::specialp :export t)
(lisp-system::array-general-p :export t)
(lisp-system::array-specialized-p :export t)
(lisp-system::simple-sort :export t)
(lisp-system::bubble-sort :export t)
(lisp-system::quick-sort :export t)
(lisp-system::merge-sort :export t)
(lisp-system::exit :export t)
(lisp-system::quit :export t)
(lisp-system::closp :export t)
(lisp-system::fixnump :export t)
(lisp-system::bignump :export t)
(lisp-system::ratiop :export t)
(lisp-system::short-float-p :export t)
(lisp-system::single-float-p :export t)
(lisp-system::double-float-p :export t)
(lisp-system::long-float-p :export t)
(lisp-system::callnamep :export t)
(lisp-system::large-number :export t)
(lisp-system::make-character :export t)
(lisp-system::make-fixnum :export t)
(lisp-system::make-bignum :export t)
(lisp-system::make-ratio :export t)
(lisp-system::make-complex :export t)
(lisp-system::equal-random-state :export t)
(lisp-system::*subtypep!* :constant system :name subtypep-value)
(lisp-system::subtypep! :export t :constant system :name subtypep-extend)
(lisp-system::subtypep-number :export t)
(lisp-system::eastasian-set :export t)
(lisp-system::eastasian-get :export t)
(lisp-system::eastasian-width :export t)
(lisp-system::run-program :export t)
(lisp-system::make-callname :export t)
(lisp-system::remove-file :export t)
(lisp-system::remove-directory :export t)
(lisp-system::declare-parse :export t)
(lisp-system::parse-type :export t)
(lisp-system::upgraded-open-element-type :export t)
(lisp-system::make-memory-input-stream :export t)
(lisp-system::make-memory-output-stream :export t)
(lisp-system::make-memory-io-stream :export t)
(lisp-system::with-input-from-memory :export t)
(lisp-system::with-output-to-memory :export t)
(lisp-system::get-output-stream-memory :export t)
(lisp-system::memory-stream-p :export t)
(lisp-system::byte-integer :export t)
(lisp-system::sysctl :export t)
(lisp-system::terme :export t)
(lisp-system::fpclassify :export t)
(lisp-system::make-paper :export t)
(lisp-system::info-paper :export t)
(lisp-system::array-paper :export t)
(lisp-system::body-paper :export t)
(lisp-system::dlfile :export t)
(lisp-system::dlcall :export t)

(lisp-system::*standard-input* :name standard-input :constant system)
(lisp-system::*standard-output* :name standard-output :constant system)
(lisp-system::*standard-error* :name standard-error :constant system)

