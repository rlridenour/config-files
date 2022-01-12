
:tanat

"28.0.50"

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2022-01-11 11:03:20" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2022-01-11 11:03:20" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2022-01-11 11:03:20" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "el-get" ("2022-01-11 11:03:20" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2022-01-11 11:03:20" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2022-01-11 11:03:20" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "use-package" ("2022-01-11 11:03:20" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2022-01-11 11:03:20" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "use-package-ensure-system-package" ("2022-01-11 11:03:20" ("use-package" "system-packages") (:flavor melpa :files ("use-package-ensure-system-package.el" "use-package-ensure-system-package-pkg.el") :package "use-package-ensure-system-package" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "system-packages" ("2022-01-11 11:03:20" ("emacs") (:type git :flavor melpa :host gitlab :repo "jabranham/system-packages" :package "system-packages" :local-repo "system-packages")) "use-package-chords" ("2022-01-11 11:03:20" ("use-package" "bind-key" "bind-chord" "key-chord") (:flavor melpa :files ("use-package-chords.el" "use-package-chords-pkg.el") :package "use-package-chords" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "bind-chord" ("2022-01-11 11:03:20" ("bind-key" "key-chord") (:flavor melpa :files ("bind-chord.el" "bind-chord-pkg.el") :package "bind-chord" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "key-chord" ("2022-01-11 11:03:20" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/key-chord" :package "key-chord" :local-repo "key-chord")) "exec-path-from-shell" ("2022-01-11 11:03:20" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "purcell/exec-path-from-shell" :package "exec-path-from-shell" :local-repo "exec-path-from-shell")) "hc-zenburn-theme" ("2022-01-11 11:03:20" nil (:type git :flavor melpa :files ("hc-zenburn-theme.el" "hc-zenburn-theme-pkg.el") :host github :repo "edran/hc-zenburn-emacs" :package "hc-zenburn-theme" :local-repo "hc-zenburn-emacs")) "modus-themes" ("2022-01-11 11:03:20" ("emacs") (:host gitlab :repo "protesilaos/modus-themes" :branch "main" :flavor melpa :package "modus-themes" :type git :local-repo "modus-themes"))))

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight-x straight-autoloads straight) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t nil) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t nil) (autoload 'straight-dependents "straight" "Return a list PACKAGE's dependents.

(fn &optional PACKAGE)" t nil) (register-definition-prefixes "straight" '("straight-")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "bind-key" ((bind-key-autoloads bind-key) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t nil) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings")) (provide 'bind-key-autoloads)) "use-package" ((use-package-diminish use-package use-package-lint use-package-delight use-package-jump use-package-autoloads use-package-core use-package-ensure use-package-bind-key) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil) (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)" nil nil) (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil) (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil) (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*")) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function '1) (register-definition-prefixes "use-package-core" '("use-package-")) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)" nil nil) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight")) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish")) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)" nil nil) (register-definition-prefixes "use-package-ensure" '("use-package-")) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

(fn PACKAGE)" t nil) (register-definition-prefixes "use-package-jump" '("use-package-find-require")) (autoload 'use-package-lint "use-package-lint" "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration")) (provide 'use-package-autoloads)) "system-packages" ((system-packages-autoloads system-packages) (autoload 'system-packages-install "system-packages" "Install system packages.

Use the package manager from `system-packages-package-manager' to
install PACK.  You may use ARGS to pass options to the package
manger.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-ensure "system-packages" "Ensure PACK is installed on system.
Search for PACK with `system-packages-package-installed-p', and
install the package if not found.  Use ARGS to pass options to
the package manager.

(fn PACK &optional ARGS)" t nil) (defalias 'system-packages-package-installed-p #'executable-find "Return t if PACK is installed.
Currently an alias for `executable-find', so it will give wrong
results if the package and executable names are different.") (autoload 'system-packages-search "system-packages" "Search for system packages.

Use the package manager named in `system-packages-package-manager'
to search for PACK.  You may use ARGS to pass options to the
package manager.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-uninstall "system-packages" "Uninstall system packages.

Uses the package manager named in
`system-packages-package-manager' to uninstall PACK.  You may use
ARGS to pass options to the package manager.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-list-dependencies-of "system-packages" "List the dependencies of PACK.

You may use ARGS to pass options to the package manager.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-get-info "system-packages" "Display local package information of PACK.

With a prefix argument, display remote package information.  You
may use ARGS to pass options to the package manager.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-list-files-provided-by "system-packages" "List the files provided by PACK.

You may use ARGS to pass options to the package manager.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-owning-file "system-packages" "Search for packages containing FILE.

Search only locally installed packages by default.  With a prefix
argument, try to search packages not yet installed.

You may use ARGS to pass options to the package manager.

(fn FILE &optional ARGS)" t nil) (autoload 'system-packages-change-log "system-packages" "Show the change log of PACK.

You may use ARGS to pass options to the package manager.

(fn PACK &optional ARGS)" t nil) (autoload 'system-packages-update "system-packages" "Update system packages.

Use the package manager `system-packages-package-manager'.  You
may use ARGS to pass options to the package manger.

(fn &optional ARGS)" t nil) (autoload 'system-packages-remove-orphaned "system-packages" "Remove orphaned packages.

Uses the package manager named in
`system-packages-package-manager'.  You may use ARGS to pass
options to the package manger.

(fn &optional ARGS)" t nil) (autoload 'system-packages-list-installed-packages "system-packages" "List explicitly installed packages.

Uses the package manager named in
`system-packages-package-manager'.  With
\\[universal-argument] (for ALL), list all installed packages.
You may use ARGS to pass options to the package manger.

(fn ALL &optional ARGS)" t nil) (autoload 'system-packages-clean-cache "system-packages" "Clean the cache of the package manager.

You may use ARGS to pass options to the package manger.

(fn &optional ARGS)" t nil) (autoload 'system-packages-log "system-packages" "Show a log from `system-packages-package-manager'.

You may use ARGS to pass options to the package manger.

(fn &optional ARGS)" t nil) (autoload 'system-packages-verify-all-packages "system-packages" "Check that files owned by packages are present on the system.

You may use ARGS to pass options to the package manger.

(fn &optional ARGS)" t nil) (autoload 'system-packages-verify-all-dependencies "system-packages" "Verify that all required dependencies are installed on the system.

You may use ARGS to pass options to the package manger.

(fn &optional ARGS)" t nil) (register-definition-prefixes "system-packages" '("system-packages-")) (provide 'system-packages-autoloads)) "use-package-ensure-system-package" ((use-package-ensure-system-package use-package-ensure-system-package-autoloads) (autoload 'use-package-normalize/:ensure-system-package "use-package-ensure-system-package" "Turn `arg' into a list of cons-es of (`package-name' . `install-command').

(fn NAME-SYMBOL KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure-system-package "use-package-ensure-system-package" "Execute the handler for `:ensure-system-package' keyword in `use-package'.

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-ensure-system-package" '("use-package-ensure-system-package-")) (provide 'use-package-ensure-system-package-autoloads)) "key-chord" ((key-chord-autoloads key-chord) (defvar key-chord-mode nil "Non-nil if Key-Chord mode is enabled.
See the `key-chord-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `key-chord-mode'.") (custom-autoload 'key-chord-mode "key-chord" nil) (autoload 'key-chord-mode "key-chord" "Map pairs of simultaneously pressed keys to commands.

This is a minor mode.  If called interactively, toggle the
`Key-Chord mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'key-chord-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

See functions `key-chord-define-global', `key-chord-define-local',
and `key-chord-define' and variables `key-chord-two-keys-delay'
and `key-chord-one-key-delay'.

(fn &optional ARG)" t nil) (autoload 'key-chord-define-global "key-chord" "Define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

Note that KEYS defined locally in the current buffer will have
precedence.

(fn KEYS COMMAND)" t nil) (autoload 'key-chord-define-local "key-chord" "Locally define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode.

(fn KEYS COMMAND)" t nil) (autoload 'key-chord-define "key-chord" "Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

(fn KEYMAP KEYS COMMAND)" nil nil) (register-definition-prefixes "key-chord" '("key-chord-")) (provide 'key-chord-autoloads)) "bind-chord" ((bind-chord bind-chord-autoloads) (autoload 'bind-chord "bind-chord" "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed).

(fn CHORD COMMAND &optional KEYMAP)" nil t) (autoload 'bind-chords "bind-chord" "Bind multiple chords at once.

Accepts keyword argument:
:map - a keymap into which the keybindings should be added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (provide 'bind-chord-autoloads)) "use-package-chords" ((use-package-chords use-package-chords-autoloads) (defalias 'use-package-autoloads/:chords 'use-package-autoloads-mode) (defalias 'use-package-normalize/:chords 'use-package-normalize-binder) (autoload 'use-package-handler/:chords "use-package-chords" "Handler for `:chords' keyword in `use-package'.

(fn NAME KEYWORD ARG REST STATE)" nil nil) (provide 'use-package-chords-autoloads)) "exec-path-from-shell" ((exec-path-from-shell exec-path-from-shell-autoloads) (autoload 'exec-path-from-shell-copy-envs "exec-path-from-shell" "Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
The result is an alist, as described by
`exec-path-from-shell-getenvs'.

(fn NAMES)" nil nil) (autoload 'exec-path-from-shell-copy-env "exec-path-from-shell" "Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
Return the value of the environment variable.

(fn NAME)" t nil) (autoload 'exec-path-from-shell-initialize "exec-path-from-shell" "Initialize environment from the user's shell.

The values of all the environment variables named in
`exec-path-from-shell-variables' are set from the corresponding
values used in the user's shell." t nil) (register-definition-prefixes "exec-path-from-shell" '("exec-path-from-shell-")) (provide 'exec-path-from-shell-autoloads)) "hc-zenburn-theme" ((hc-zenburn-theme-autoloads hc-zenburn-theme) (and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "hc-zenburn-theme" '("hc-zenburn")) (provide 'hc-zenburn-theme-autoloads)) "modus-themes" ((modus-themes modus-vivendi-theme modus-operandi-theme modus-themes-autoloads) (register-definition-prefixes "modus-operandi-theme" '("modus-operandi")) (autoload 'modus-themes-contrast "modus-themes" "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB.

(fn C1 C2)" nil nil) (autoload 'modus-themes-color "modus-themes" "Return color value for COLOR from current palette.
COLOR is a key in `modus-themes-operandi-colors' or
`modus-themes-vivendi-colors'.

(fn COLOR)" nil nil) (autoload 'modus-themes-color-alts "modus-themes" "Return color value from current palette.
When Modus Operandi is enabled, return color value for color
LIGHT-COLOR.  When Modus Vivendi is enabled, return color value
for DARK-COLOR.  LIGHT-COLOR and DARK-COLOR are keys in
`modus-themes-operandi-colors' or `modus-themes-vivendi-colors'.

(fn LIGHT-COLOR DARK-COLOR)" nil nil) (autoload 'modus-themes-load-themes "modus-themes" "Ensure that the Modus themes are in `custom-enabled-themes'.

This function is intended for use in package declarations such as
those defined with the help of `use-package'.  The idea is to add
this function to the `:init' stage of the package's loading, so
that subsequent calls that assume the presence of a loaded theme,
like `modus-themes-toggle' or `modus-themes-load-operandi', will
continue to work as intended even if they are lazy-loaded (such
as when they are declared in the `:config' phase)." nil nil) (autoload 'modus-themes-load-operandi "modus-themes" "Load `modus-operandi' and disable `modus-vivendi'.
Also run `modus-themes-after-load-theme-hook'." t nil) (autoload 'modus-themes-load-vivendi "modus-themes" "Load `modus-vivendi' and disable `modus-operandi'.
Also run `modus-themes-after-load-theme-hook'." t nil) (autoload 'modus-themes-toggle "modus-themes" "Toggle between `modus-operandi' and `modus-vivendi' themes.
Also runs `modus-themes-after-load-theme-hook' at its last stage
by virtue of calling either of `modus-themes-load-operandi' and
`modus-themes-load-vivendi' functions." t nil) (when load-file-name (let ((dir (file-name-directory load-file-name))) (unless (equal dir (expand-file-name "themes/" data-directory)) (add-to-list 'custom-theme-load-path dir)))) (register-definition-prefixes "modus-themes" '("modus-themes-")) (register-definition-prefixes "modus-vivendi-theme" '("modus-vivendi")) (provide 'modus-themes-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 11 "melpa" nil "gnu-elpa-mirror" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "use-package" nil "bind-key" nil "use-package-ensure-system-package" nil "system-packages" nil "use-package-chords" nil "bind-chord" nil "key-chord" nil "exec-path-from-shell" nil "cl-lib" nil "hc-zenburn-theme" nil "modus-themes" nil)) melpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "use-package-ensure-system-package" (use-package-ensure-system-package :type git :flavor melpa :files ("use-package-ensure-system-package.el" "use-package-ensure-system-package-pkg.el") :host github :repo "jwiegley/use-package") "system-packages" (system-packages :type git :flavor melpa :host gitlab :repo "jabranham/system-packages") "use-package-chords" (use-package-chords :type git :flavor melpa :files ("use-package-chords.el" "use-package-chords-pkg.el") :host github :repo "jwiegley/use-package") "bind-chord" (bind-chord :type git :flavor melpa :files ("bind-chord.el" "bind-chord-pkg.el") :host github :repo "jwiegley/use-package") "key-chord" (key-chord :type git :flavor melpa :host github :repo "emacsorphanage/key-chord") "exec-path-from-shell" (exec-path-from-shell :type git :flavor melpa :host github :repo "purcell/exec-path-from-shell") "cl-lib" nil "hc-zenburn-theme" (hc-zenburn-theme :type git :flavor melpa :files ("hc-zenburn-theme.el" "hc-zenburn-theme-pkg.el") :host github :repo "edran/hc-zenburn-emacs") "modus-themes" (modus-themes :type git :flavor melpa :branch "main" :host gitlab :repo "protesilaos/modus-themes"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "el-get" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "use-package-ensure-system-package" "system-packages" "use-package-chords" "bind-chord" "key-chord" "exec-path-from-shell" "cl-lib" "hc-zenburn-theme" "modus-themes")

t
