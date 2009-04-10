;;; DO NOT MODIFY THIS FILE
(if (featurep 'edict-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "edict/_pkg.el")

(if (fboundp 'package-provide)
    (package-provide 'edict :version 1.01 :type 'regular))

;;;***

;;;### (autoloads (dui-describe-method dui-invoke-edit-method dui-invoke-insert-method dui-invoke-search-method dl-mode) "dui" "edict/dui.el")

(autoload 'dl-mode "dui" "\
Minor mode for dictionary lookup, with interfaces to dictionary utilities.

Null ARG toggles the mode, positive ARG turns the mode on, negative ARG
turns the mode off.

When the mode is already off, (dl-mode -1) has the side effect of checking
and reporting any conflicting bindings.

\\{dl-mode-map}" t nil)

(autoload 'dui-invoke-search-method "dui" "\
Invokes a dictionary lookup method.

If ASK is non-nil, reads a method from the minibuffer.  Otherwise invokes the
current default search method.

\\[dui-describe-method] gives help for individual methods." t nil)

(autoload 'dui-invoke-insert-method "dui" "\
Invokes a method to add a dictionary entry.

If ASK is non-nil, reads a method from the minibuffer.  Otherwise invokes the
current default insert method.

\\[dui-describe-method] gives help for individual methods." t nil)

(autoload 'dui-invoke-edit-method "dui" "\
Invokes a dictionary editing method.

If ASK is non-nil, reads a method from the minibuffer.  Otherwise invokes the
current default edit method.

\\[dui-describe-method] gives help for individual methods." t nil)

(autoload 'dui-describe-method "dui" "\
Shows the docstring for METHOD (a string) in a temporary buffer." t nil)

;;;***

;;;### (autoloads (edict-add-kanji edict-add-english edict-add-word edict-edit-mode) "edict-edit" "edict/edict-edit.el")

(autoload 'edict-edit-mode "edict-edit" "\
Major mode for editing edict entries.
TAB      Tab to next field in this entry.
RETURN   Start a new entry on the next line.
c-A      Edit the kanji field, and start entering kanji.
c-E      Go to the end, and start editing english.
C-c C-c  Install the edited changes & save the file.
C-x C-s  Install the edited changes & save the file.
" t nil)

(autoload 'edict-add-word "edict-edit" "\
Add any word to the private dictionary." t nil)

(autoload 'edict-add-english "edict-edit" "\
Add the english word at point to the dictionary." t nil)

(autoload 'edict-add-kanji "edict-edit" "\
Add the region as a kanji entry in the dictionary." t nil)

;;;***

;;;### (autoloads (edict-search-kanji edict-search-english edict-force-init edict-version) "edict" "edict/edict.el")

(autoload 'edict-version "edict" "\
The function edict-version simply displays (as a message in the
mini-buffer) the version of the edict software that you are running
at the moment.  The same string is also returned from the function." t nil)

(autoload 'edict-force-init "edict" "\
Reread the edict files even if edict-buffer exists.

Useful when you have updated the edict-dictionaries variable or corrupted
the edict buffer." t nil)

(autoload 'edict-search-english "edict" "\
Attempts to translate the english word we are looking at. Picks the word 
in the same way as ispell, ie backs up from whitespace, and then expands.

Result is presented in a window that is not selected. Clear the window by
using a negative prefix argument.

If given an argument, adds an english word to the private dictionary." t nil)

(autoload 'edict-search-kanji "edict" "\
Attempts to translate the Kanji sequence between mark and point.

Result is presented in a window that is not selected. Clear the window
with for instance C-X 1

Given a numeric argument, this adds the Kanji sequence to the user's
private dictionary.

If all searches fail, initialization may be bogus.  See the documentation
for `edict-init'." t nil)

;;;***

(provide 'edict-autoloads)
