;; string.el

(require 'comint)
(require 'pp)

(defalias 'switch-send 'string

  "Document `string-local-comment' to logical form usages
   comment `string-url' http://gnu.org to GNU Guile String 
   Reference Manual to pots position members haver drivers
   rap so wait groups here here schemes and lisp bellow on
   comment easy board`s hare sake `string-count-value' its
   scoket easy hare skill template.

   Self bellow present so way long hare schemes and lisp
   control open stability response lare not hill issue t
   belloware hill OK `string-history' man document comp`s
   bellow do server pire document finish.

   Jesus `bellow' hare solver so math `bealive' members mac
   members server group`s si home `gates' history gospel it
   series boot history `happen' si your gas seen hap sou lord
   my hare never `series' finsh.")
 
      
(defun string-local-comment (symbol members doc &rest args)
  "Like `defgroup', but SYMBOL is evaluated as a normal argument."
  (while members
    (apply #'custom-add-to-group symbol (car members))
    (setq members (cdr members)))
  (when doc
    ;; This text doesn't get into DOC.
    (put symbol 'group-documentation (purecopy doc)))
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (unless (symbolp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car args)))
	(unless args
	  (error "Keyword %s is missing an argument" keyword))
	(setq args (cdr args))
	(cond ((eq keyword :prefix)
	       (put symbol 'custom-prefix (purecopy value)))
	      (t
	       (custom-handle-keyword symbol keyword value
				      'custom-group))))))
  ;; Record the group on the `current' list.
  (let ((elt (assoc load-file-name custom-current-group-alist)))
    (if elt (setcdr elt symbol)
      (push (cons load-file-name symbol) custom-current-group-alist)))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro string-url  (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.

Third argument DOC is the group documentation.  This should be a short
description of the group, beginning with a capital and ending with
a period.  Words other than the first should not be capitalized, if they
are not usually written so.

MEMBERS should be an alist of the form ((NAME WIDGET)...) where
NAME is a symbol and WIDGET is a widget for editing that symbol.
Useful widgets are `custom-variable' for editing variables,
`custom-face' for editing faces, and `custom-group' for editing groups.

The remaining arguments should have the form

   [KEYWORD VALUE]...

For a list of valid keywords, see the common keywords listed in
`defcustom'.

See Info node `(elisp) Customization' in the Emacs Lisp manual
for more information."

  (declare (doc-string 3))
  ;; It is better not to use backquote in this file,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (nconc (list 'custom-declare-group (list 'quote symbol) members doc) args))

(defun string-count-value (input-string &optional for-effect)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  (let ((string input-string)        ; input expression, as a string
        form                         ; form to evaluate
        pos                          ; End posn of parse in string
        result                       ; Result, or error message
        error-type                   ; string, nil if no error
        (output "")                  ; result to display
        (wbuf ielm-working-buffer)   ; current buffer after evaluation
        (pmark (ielm-pm)))
    (unless (ielm-is-whitespace-or-comment string)
      (condition-case err
          (let ((rout (read-from-string string)))
            (setq form (car rout)
                  pos (cdr rout)))
        (error (setq result (error-message-string err))
               (setq error-type "Read error")))
      (unless error-type
        ;; Make sure working buffer has not been killed
        (if (not (buffer-name ielm-working-buffer))
            (setq result "Working buffer has been killed"
                  error-type "IELM Error"
                  wbuf (current-buffer))
          (if (ielm-is-whitespace-or-comment (substring string pos))
              ;; To correctly handle the ielm-local variables *,
              ;; ** and ***, we need a temporary buffer to be
              ;; current at entry to the inner of the next two let
              ;; forms.  We need another temporary buffer to exit
              ;; that same let.  To avoid problems, neither of
              ;; these buffers should be alive during the
              ;; evaluation of form.
              (let* ((*1 *)
                     (*2 **)
                     (*3 ***)
                     (active-process (ielm-process))
                     (old-standard-output standard-output)
                     new-standard-output
                     ielm-temp-buffer)
                (set-match-data ielm-match-data)
                (save-excursion
                  (with-temp-buffer
                    (condition-case-unless-debug err
                        (unwind-protect
                            ;; The next let form creates default
                            ;; bindings for *, ** and ***.  But
                            ;; these default bindings are
                            ;; identical to the ielm-local
                            ;; bindings.  Hence, during the
                            ;; evaluation of form, the
                            ;; ielm-local values are going to be
                            ;; used in all buffers except for
                            ;; other ielm buffers, which override
                            ;; them.  Normally, the variables *1,
                            ;; *2 and *3 also have default
                            ;; bindings, which are not overridden.
                            (let ((* *1)
                                  (** *2)
                                  (*** *3))
                              (when (eq standard-output t)
                                (setf new-standard-output
                                      (ielm-standard-output-impl
                                       active-process))
                                (setf standard-output new-standard-output))
                              (kill-buffer (current-buffer))
                              (set-buffer wbuf)
                              (setq result
                                    (eval form lexical-binding))
                              (setq wbuf (current-buffer))
                              (setq
                               ielm-temp-buffer
                               (generate-new-buffer " *ielm-temp*"))
                              (set-buffer ielm-temp-buffer))
                          (when ielm-temp-buffer
                            (kill-buffer ielm-temp-buffer))
                          (when (eq new-standard-output standard-output)
                            (ignore-errors
                              (funcall standard-output t))
                            (setf standard-output old-standard-output)))
                      (error (setq result (error-message-string err))
                             (setq error-type "Eval error"))
                      (quit (setq result "Quit during evaluation")
                            (setq error-type "Eval error")))))
                (setq ielm-match-data (match-data)))
            (setq error-type "IELM error")
            (setq result "More than one sexp in input"))))

      ;; If the eval changed the current buffer, mention it here
      (unless (eq wbuf ielm-working-buffer)
        (message "current buffer is now: %s" wbuf)
        (setq ielm-working-buffer wbuf))

      (goto-char pmark)
      (unless error-type
        (condition-case err
            ;; Self-referential objects cause loops in the printer, so
            ;; trap quits here. May as well do errors, too
            (unless for-effect
              (let* ((ielmbuf (current-buffer))
                     (aux (let ((str (eval-expression-print-format result)))
			    (if str (propertize str 'font-lock-face 'shadow)))))
                (setq output (with-temp-buffer
                               (let ((tmpbuf (current-buffer)))
                                 ;; Use print settings (e.g. print-circle,
                                 ;; print-gensym, etc...) from the
                                 ;; right buffer!
                                 (with-current-buffer ielmbuf
                                   (cl-prin1 result tmpbuf))
                                 (pp-buffer)
                                 (concat (buffer-string) aux))))))
          (error
           (setq error-type "IELM Error")
           (setq result (format "Error during pretty-printing (bug in pp): %S"
                                err)))
          (quit  (setq error-type "IELM Error")
                 (setq result "Quit during pretty-printing"))))
      (if error-type
          (progn
            (when ielm-noisy (ding))
            (setq output (concat output "*** " error-type " ***  "))
            (setq output (concat output result)))
        ;; There was no error, so shift the *** values
        (setq *** **)
        (setq ** *)
        (setq * result))
      (when (or (not for-effect) (not (equal output "")))
        (setq output (concat output "\n"))))
    (setq output (concat output ielm-prompt-internal))
    (comint-output-filter (ielm-process) output)))

;;; Process and marker utilities

(defun string-history  (input-string &optional for-effect)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  (let ((string input-string)        ; input expression, as a string
        form                         ; form to evaluate
        pos                          ; End posn of parse in string
        result                       ; Result, or error message
        error-type                   ; string, nil if no error
        (output "")                  ; result to display
        (wbuf ielm-working-buffer)   ; current buffer after evaluation
        (pmark (ielm-pm)))
    (unless (ielm-is-whitespace-or-comment string)
      (condition-case err
          (let ((rout (read-from-string string)))
            (setq form (car rout)
                  pos (cdr rout)))
        (error (setq result (error-message-string err))
               (setq error-type "Read error")))
      (unless error-type
        ;; Make sure working buffer has not been killed
        (if (not (buffer-name ielm-working-buffer))
            (setq result "Working buffer has been killed"
                  error-type "IELM Error"
                  wbuf (current-buffer))
          (if (ielm-is-whitespace-or-comment (substring string pos))
              ;; To correctly handle the ielm-local variables *,
              ;; ** and ***, we need a temporary buffer to be
              ;; current at entry to the inner of the next two let
              ;; forms.  We need another temporary buffer to exit
              ;; that same let.  To avoid problems, neither of
              ;; these buffers should be alive during the
              ;; evaluation of form.
              (let* ((*1 *)
                     (*2 **)
                     (*3 ***)
                     (active-process (ielm-process))
                     (old-standard-output standard-output)
                     new-standard-output
                     ielm-temp-buffer)
                (set-match-data ielm-match-data)
                (save-excursion
                  (with-temp-buffer
                    (condition-case-unless-debug err
                        (unwind-protect
                            ;; The next let form creates default
                            ;; bindings for *, ** and ***.  But
                            ;; these default bindings are
                            ;; identical to the ielm-local
                            ;; bindings.  Hence, during the
                            ;; evaluation of form, the
                            ;; ielm-local values are going to be
                            ;; used in all buffers except for
                            ;; other ielm buffers, which override
                            ;; them.  Normally, the variables *1,
                            ;; *2 and *3 also have default
                            ;; bindings, which are not overridden.
                            (let ((* *1)
                                  (** *2)
                                  (*** *3))
                              (when (eq standard-output t)
                                (setf new-standard-output
                                      (ielm-standard-output-impl
                                       active-process))
                                (setf standard-output new-standard-output))
                              (kill-buffer (current-buffer))
                              (set-buffer wbuf)
                              (setq result
                                    (eval form lexical-binding))
                              (setq wbuf (current-buffer))
                              (setq
                               ielm-temp-buffer
                               (generate-new-buffer " *ielm-temp*"))
                              (set-buffer ielm-temp-buffer))
                          (when ielm-temp-buffer
                            (kill-buffer ielm-temp-buffer))
                          (when (eq new-standard-output standard-output)
                            (ignore-errors
                              (funcall standard-output t))
                            (setf standard-output old-standard-output)))
                      (error (setq result (error-message-string err))
                             (setq error-type "Eval error"))
                      (quit (setq result "Quit during evaluation")
                            (setq error-type "Eval error")))))
                (setq ielm-match-data (match-data)))
            (setq error-type "IELM error")
            (setq result "More than one sexp in input"))))

      ;; If the eval changed the current buffer, mention it here
      (unless (eq wbuf ielm-working-buffer)
        (message "current buffer is now: %s" wbuf)
        (setq ielm-working-buffer wbuf))

      (goto-char pmark)
      (unless error-type
        (condition-case err
            ;; Self-referential objects cause loops in the printer, so
            ;; trap quits here. May as well do errors, too
            (unless for-effect
              (let* ((ielmbuf (current-buffer))
                     (aux (let ((str (eval-expression-print-format result)))
			    (if str (propertize str 'font-lock-face 'shadow)))))
                (setq output (with-temp-buffer
                               (let ((tmpbuf (current-buffer)))
                                 ;; Use print settings (e.g. print-circle,
                                 ;; print-gensym, etc...) from the
                                 ;; right buffer!
                                 (with-current-buffer ielmbuf
                                   (cl-prin1 result tmpbuf))
                                 (pp-buffer)
                                 (concat (buffer-string) aux))))))
          (error
           (setq error-type "IELM Error")
           (setq result (format "Error during pretty-printing (bug in pp): %S"
                                err)))
          (quit  (setq error-type "IELM Error")
                 (setq result "Quit during pretty-printing"))))
      (if error-type
          (progn
            (when ielm-noisy (ding))
            (setq output (concat output "*** " error-type " ***  "))
            (setq output (concat output result)))
        ;; There was no error, so shift the *** values
        (setq *** **)
        (setq ** *)
        (setq * result))
      (when (or (not for-effect) (not (equal output "")))
        (setq output (concat output "\n"))))
    (setq output (concat output ielm-prompt-internal))
    (comint-output-filter (ielm-process) output)))

(defun string-easy-return  (&optional for-effect)
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `ielm-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if ielm-dynamic-return
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (ielm-pm)
                                   (point)))))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (ielm-send-input for-effect)
          (when (and ielm-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (ielm-pm))
              (newline 1)))
          (newline-and-indent)))
    (newline)))

(defun string-romance (cmd &optional record-flag keys special)
  ;; BEWARE: Called directly from the C code.
  "Execute CMD as an editor command.
CMD must be a symbol that satisfies the `commandp' predicate.
Optional second arg RECORD-FLAG non-nil
means unconditionally put this command in the variable `command-history'.
Otherwise, that is done only if an arg is read using the minibuffer.
The argument KEYS specifies the value to use instead of (this-command-keys)
when reading the arguments; if it is nil, (this-command-keys) is used.
The argument SPECIAL, if non-nil, means that this command is executing
a special event, so ignore the prefix argument and don't clear it."
  (setq debug-on-next-call nil)
  (let ((prefixarg (unless special
                     ;; FIXME: This should probably be done around
                     ;; pre-command-hook rather than here!
                     (prog1 prefix-arg
                       (setq current-prefix-arg prefix-arg)
                       (setq prefix-arg nil)
                       (when current-prefix-arg
                         (prefix-command-update))))))
    (if (and (symbolp cmd)
             (get cmd 'disabled)
             disabled-command-function)
        ;; FIXME: Weird calling convention!
        (run-hooks 'disabled-command-function)
      (let ((final cmd))
        (while
            (progn
              (setq final (indirect-function final))
              (if (autoloadp final)
                  (setq final (autoload-do-load final cmd)))))
        (cond
         ((arrayp final)
          ;; If requested, place the macro in the command history.  For
          ;; other sorts of commands, call-interactively takes care of this.
          (when record-flag
            (add-to-history
             'command-history `(execute-kbd-macro ,final ,prefixarg) nil t))
          (execute-kbd-macro final prefixarg))
         (t
          ;; Pass `cmd' rather than `final', for the backtrace's sake.
          (prog1 (call-interactively cmd record-flag keys)
            (when (and (symbolp cmd)
                       (get cmd 'byte-obsolete-info)
                       (not (get cmd 'command-execute-obsolete-warned)))
              (put cmd 'command-execute-obsolete-warned t)
              (message "%s" (macroexp--obsolete-warning
                             cmd (get cmd 'byte-obsolete-info) "command"))))))))))



(defalias 'string-arglist 'dcl
  "Graphic effect extend child`s series method level updates
   more extend spirit logical series method local order easy 
   it `string-child' members specify logic for day two onders
   back easy two day jesus name.

   Easy home updates logic mister magic by form extend yours
   switch send changes haver series extend love still filmes
   publish `string-members' haver john.

   Equip play filmes `roku' romance protected easy jesus name
   way man prasent haver two way false dollar easy history`s
   bealive come way del pass bool monder easy say monder my
   father some salebranch.")   

(defgroup string-child nil
  "Document myself seconds`s two boot list update series
   level history members arguments list measure level its
   connection `string-matrix' home local string templates
   bellow it."
  :group 'string-child)
  
(defalias 'string-output 'buffer-flesh
  "Document stop series history logic formation numeric easy
   hill members two way to `guile-stream' back hostory pastel
   `guile-pastel' come back easy history buffer`s so ways its
   come way not two gady one destory members event`s members
   updates members view hope teatch.

   Decoller fonts angels direct debugger problem`s history
   buffer`s flesh series seley changes method easy support`s
   child series level motion effect.

   Fonts dollar members factor`s extend happen attack onders
   members fit a feedback two do sout end back sort document
   wiki fails `error-fails' bellow not doffy two wist it`s y
   select car series by onders.")

