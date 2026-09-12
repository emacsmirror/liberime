;;; liberime-test.el --- Integration tests for liberime -*- lexical-binding: t; -*-

;; Copyright (C) 2024 jixiuf

;; Author: jixiuf

;;; Commentary:

;; Integration test suite that exercises the real C dynamic module
;; (liberime-core) with a running librime instance.  These tests
;; require librime to be installed and the C module to be compiled.
;;
;; Run with:
;;   make test
;;
;; Or manually:
;;   emacs --batch -Q -L . -l ert -l liberime-test.el -f liberime-test-run

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'liberime)

;; ---------------------------------------------------------------------------
;; Test setup / teardown
;; ---------------------------------------------------------------------------

(defvar liberime-test--session-id nil
  "Session ID from liberime-start for test use.")

(defvar liberime-test--shared-dir nil
  "Shared data dir used for tests.")

(defvar liberime-test--user-dir nil
  "Temporary user data dir for tests.")

(defun liberime-test--setup ()
  "Initialize librime for testing.
Creates a temporary user data directory."
  (unless liberime-test--session-id
    (setq liberime-test--user-dir
          (make-temp-file "liberime-test-user" t))
    (setq liberime-test--shared-dir
          (or (liberime-get-shared-data-dir)
              ;; Fallback: try common paths
              (cl-find-if #'file-directory-p
                          '("/usr/share/rime-data"
                            "/usr/local/share/rime-data"
                            "/usr/share/local/rime-data"))))
    (when liberime-test--shared-dir
      (setq liberime-test--session-id
            (ignore-errors
              (liberime-start liberime-test--shared-dir
                              liberime-test--user-dir)))
      ;; Select a schema for testing (prefer luna_pinyin for
      ;; deterministic labels; fall back to the first available)
      (when liberime-test--session-id
        (let* ((schemas (liberime-get-schema-list))
               (ids (mapcar #'car schemas))
               (schema (if (member "luna_pinyin" ids)
                           "luna_pinyin"
                         (caar schemas))))
          (when schema
            (liberime-select-schema schema)))))))

(defun liberime-test--teardown ()
  "Finalize librime and clean up."
  (when liberime-test--session-id
    (ignore-errors (liberime-finalize))
    (setq liberime-test--session-id nil))
  (when (and liberime-test--user-dir
             (file-directory-p liberime-test--user-dir))
    (delete-directory liberime-test--user-dir t)
    (setq liberime-test--user-dir nil)))

(defun liberime-test--skip-unless-rime ()
  "Skip the current test if librime is not available."
  (unless (featurep 'liberime-core)
    (ert-skip "liberime-core module not loaded"))
  (liberime-test--setup)
  (unless liberime-test--session-id
    (ert-skip "Cannot initialize librime (no shared data?)")))

;; ---------------------------------------------------------------------------
;; Session management tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-start-returns-session-id ()
  "liberime-start should return t on success."
  (liberime-test--skip-unless-rime)
  (should liberime-test--session-id))

;; ---------------------------------------------------------------------------
;; Schema tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-get-schema-list ()
  "liberime-get-schema-list should return a non-empty list."
  (liberime-test--skip-unless-rime)
  (let ((schemas (liberime-get-schema-list)))
    (should (listp schemas))
    (should (> (length schemas) 0))
    (let ((schema (car schemas)))
      (should (consp schema))
      (should (stringp (car schema)))
      (should (stringp (cadr schema))))))

(ert-deftest liberime-test-search-with-schema ()
  "liberime-search with SCHEMA_ID uses that schema in a temporary
session without changing the schema of the default session."
  (liberime-test--skip-unless-rime)
  (let* ((schemas (liberime-get-schema-list))
         (ids (mapcar #'car schemas)))
    (when (>= (length ids) 2)
      (let* ((current (liberime-get-schema-config "" "schema/schema_id"))
             (target (if (string= current (nth 0 ids))
                         (nth 1 ids)
                       (nth 0 ids)))
             (result (liberime-search "wode" nil nil target)))
        ;; Search with explicit schema returns candidates
        (should (listp result))
        ;; Default session schema is unchanged
        (should (string= current
                         (liberime-get-schema-config ""
                                                     "schema/schema_id")))))))

(ert-deftest liberime-test-session-create-and-search ()
  "liberime-session-create returns a live session usable by liberime-search."
  (liberime-test--skip-unless-rime)
  (let* ((schemas (liberime-get-schema-list))
         (ids (mapcar #'car schemas))
         (session (liberime-session-create (car ids))))
    (should (integerp session))
    ;; search reusing this session reports its schema
    (let ((result (liberime-search "wode" 5 nil nil t session)))
      (should (string= (plist-get result :schema-id) (car ids))))
    (liberime-session-destroy session)
    ;; destroyed session must be rejected
    (should-error (liberime-search "wode" 5 nil nil t session))))

(ert-deftest liberime-test-session-schemas-independent ()
  "Two temporary sessions can use different schemas."
  (liberime-test--skip-unless-rime)
  (let* ((schemas (liberime-get-schema-list))
         (ids (mapcar #'car schemas)))
    (when (>= (length ids) 2)
      (let ((s1 (liberime-session-create (nth 0 ids)))
            (s2 (liberime-session-create (nth 1 ids))))
        (unwind-protect
            (progn
              (should (string=
                       (plist-get (liberime-search "wode" 5 nil nil t s1)
                                  :schema-id)
                       (nth 0 ids)))
              (should (string=
                       (plist-get (liberime-search "wode" 5 nil nil t s2)
                                  :schema-id)
                       (nth 1 ids))))
          (liberime-session-destroy s1)
          (liberime-session-destroy s2))))))

(ert-deftest liberime-test-session-functions-with-session ()
  "process-key/get-input/get-status accept a SESSION argument."
  (liberime-test--skip-unless-rime)
  (let* ((schemas (liberime-get-schema-list))
         (session (liberime-session-create (caar schemas))))
    (unwind-protect
        (progn
          (should (eq (liberime-process-key ?a 0 session) t))
          (should (string= (liberime-get-input session) "a"))
          (should (string= (alist-get 'schema_id (liberime-get-status session))
                          (caar schemas))))
      (liberime-session-destroy session))))

(ert-deftest liberime-test-search-without-candidates ()
  "Search paths must not crash on codes with few or no candidates.

Exercises `_get_candidates' with an (possibly) empty result: the linked
list nodes must be safely freeable even when never filled (see
free_candidate_list).  The assertion only checks the return type so the
test passes regardless of whether `vbnm' has candidates in the loaded
schema."
  (liberime-test--skip-unless-rime)
  ;; plain search path
  (should (listp (liberime-search "vbnm" nil)))
  ;; full-context path
  (let ((result (liberime-search "vbnm" nil nil nil t)))
    (should (listp result))))

(ert-deftest liberime-test-search-with-invalid-schema ()
  "liberime-search with unknown SCHEMA_ID should signal an error."
  (liberime-test--skip-unless-rime)
  (should-error (liberime-search "wode" nil nil "no_such_schema_xyz")))

(ert-deftest liberime-test-search-full-context ()
  "liberime-search with FULL-CONTEXT returns a plist of candidate groups."
  (liberime-test--skip-unless-rime)
  (let ((result (liberime-search "wode" nil nil nil t)))
    (should (listp result))
    (should (plist-get result :full))
    (should (listp (plist-get result :full)))
    ;; "wode" splits into "wo" + "de", so single-character
    ;; candidates only consume the shortest prefix.
    (should (plist-get result :prefix))
    (should (string= (plist-get result :remainder) "de"))
    ;; The status of the temporary session is reported.
    (should (stringp (plist-get result :schema-id)))
    (should (member (plist-get result :is-ascii-mode) '(nil t)))
    (should (member (plist-get result :is-simplified) '(nil t)))))

(ert-deftest liberime-test-search-full-context-schema ()
  "liberime-search with FULL-CONTEXT and SCHEMA_ID reports that schema."
  (liberime-test--skip-unless-rime)
  (let* ((schemas (liberime-get-schema-list))
         (ids (mapcar #'car schemas)))
    (when ids
      (let ((result (liberime-search "wode" nil nil (car ids) t)))
        (should (string= (plist-get result :schema-id) (car ids)))))))

(ert-deftest liberime-test-search-schema-leaves-no-user-config-trace ()
  "liberime-search with SCHEMA_ID restores previously_selected_schema and
schema_access_time in the user config."
  (liberime-test--skip-unless-rime)
  (let* ((schemas (liberime-get-schema-list))
         (ids (mapcar #'car schemas)))
    (when (>= (length ids) 2)
      (let* ((current (car ids))
             (target (cadr ids))
             (access-key (format "var/schema_access_time/%s" target))
             (access-value (+ 100000 (random 99999))))
        ;; Deterministic baseline: the search must not change these values.
        (liberime-set-user-config "user" "var/previously_selected_schema"
                                  current)
        (liberime-set-user-config "user" access-key access-value "int")
        (liberime-search "wode" nil nil target t)
        (should
         (string= (liberime-get-user-config "user"
                                            "var/previously_selected_schema")
                  current))
        (should
         (= (liberime-get-user-config "user" access-key "int")
            access-value))))))

(ert-deftest liberime-test-search-full-context-plain-compat ()
  "liberime-search without FULL-CONTEXT still returns a plain list."
  (liberime-test--skip-unless-rime)
  (let ((result (liberime-search "wode" 5)))
    (should (listp result))
    (should (= (length result) 5))
    (should (stringp (car result)))))

;; ---------------------------------------------------------------------------
;; Input processing tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-process-key ()
  "liberime-process-key should return t when processing a key."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (should (eq (liberime-process-key ?a) t)))

(ert-deftest liberime-test-simulate-key-sequence ()
  "liberime-simulate-key-sequence should process key sequence."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (should (eq (liberime-simulate-key-sequence "zhong") t))
  (should (string-equal (liberime-get-input) "zhong"))
  (liberime-clear-composition))

;; ---------------------------------------------------------------------------
;; Event conversion tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-event-to-key-sequence-lowercase-ascii ()
  "Lowercase ASCII character should return itself (no braces needed)."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?a) "a"))
  (should (string= (liberime-event-to-key-sequence ?z) "z"))
  (should (string= (liberime-event-to-key-sequence ?m) "m")))

(ert-deftest liberime-test-event-to-key-sequence-uppercase-ascii ()
  "Uppercase ASCII character should return itself."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?A) "A"))
  (should (string= (liberime-event-to-key-sequence ?Z) "Z")))

(ert-deftest liberime-test-event-to-key-sequence-digits ()
  "Digit characters should return themselves."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?0) "0"))
  (should (string= (liberime-event-to-key-sequence ?5) "5"))
  (should (string= (liberime-event-to-key-sequence ?9) "9")))

(ert-deftest liberime-test-event-to-key-sequence-comma ()
  "Comma without modifiers should return comma char directly."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?,) ",")))

(ert-deftest liberime-test-event-to-key-sequence-period ()
  "Period without modifiers should return period char directly."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?.) ".")))

(ert-deftest liberime-test-event-to-key-sequence-control-modifier ()
  "Control + a (Emacs returns 1 for C-a without modifier bits)."
  (liberime-test--skip-unless-rime)
  (let ((c-a (car (listify-key-sequence (kbd "C-a")))))
    (should (string= (liberime-event-to-key-sequence c-a) "{Control+a}"))))

(ert-deftest liberime-test-event-to-key-sequence-meta-modifier ()
  "Meta modifier should produce {Meta+...} format."
  (liberime-test--skip-unless-rime)
  (let ((m-a (car (listify-key-sequence (kbd "M-a")))))
    (should (string= (liberime-event-to-key-sequence m-a) "{Meta+a}"))))

(ert-deftest liberime-test-event-to-key-sequence-shift-modifier ()
  "Shift modifier should produce {Shift+...} format."
  (liberime-test--skip-unless-rime)
  (let ((s-a (car (listify-key-sequence (kbd "S-a")))))
    (should (string= (liberime-event-to-key-sequence s-a) "{Shift+a}"))))

(ert-deftest liberime-test-event-to-key-sequence-multiple-modifiers ()
  "Multiple modifiers should all appear in output."
  (liberime-test--skip-unless-rime)
  (let ((c-m-a (car (listify-key-sequence (kbd "C-M-a")))))
    (let ((result (liberime-event-to-key-sequence c-m-a)))
      (should (string= result "{Control+Meta+a}"))
      (should (string-match "Control" result))
      (should (string-match "Meta" result))
      (should (string-match "a" result)))))

(ert-deftest liberime-test-event-to-key-sequence-direction-keys ()
  "Direction keys should return {Direction} format."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 'left) "{Left}"))
  (should (string= (liberime-event-to-key-sequence 'right) "{Right}"))
  (should (string= (liberime-event-to-key-sequence 'up) "{Up}"))
  (should (string= (liberime-event-to-key-sequence 'down) "{Down}")))

(ert-deftest liberime-test-event-to-key-sequence-navigation-keys ()
  "Navigation keys should return correct format."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 'return) "{Return}"))
  (should (string= (liberime-event-to-key-sequence 'space) "{space}"))
  (should (string= (liberime-event-to-key-sequence 'backspace) "{BackSpace}"))
  (should (string= (liberime-event-to-key-sequence 'tab) "{Tab}"))
  (should (string= (liberime-event-to-key-sequence 'escape) "{Escape}"))
  (should (string= (liberime-event-to-key-sequence 'home) "{Home}"))
  (should (string= (liberime-event-to-key-sequence 'end) "{End}"))
  (should (string= (liberime-event-to-key-sequence 'delete) "{Delete}"))
  (should (string= (liberime-event-to-key-sequence 'prior) "{Prior}"))
  (should (string= (liberime-event-to-key-sequence 'next) "{Next}")))

(ert-deftest liberime-test-event-to-key-sequence-function-keys ()
  "Function keys should return {Fn} format."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 'f1) "{F1}"))
  (should (string= (liberime-event-to-key-sequence 'f5) "{F5}"))
  (should (string= (liberime-event-to-key-sequence 'f12) "{F12}")))

(ert-deftest liberime-test-event-to-key-sequence-braces ()
  "Brace characters must use names to avoid parse ambiguity."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence ?{) "{braceleft}"))
  (should (string= (liberime-event-to-key-sequence ?}) "{braceright}")))

(ert-deftest liberime-test-event-to-key-sequence-space ()
  "Space character should return single space."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-event-to-key-sequence 32) " ")))

;; ---------------------------------------------------------------------------
;; Process event tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-process-event-integer ()
  "process-event with integer should send key sequence to librime."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (should (eq (liberime-process-event ?a) t)))

(ert-deftest liberime-test-process-event-symbol-left ()
  "process-event with 'left should send Left key to librime."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  ;; Process a key first to enter composition mode
  (liberime-process-event ?w)
  (liberime-process-event ?o)
  ;; Then send space to select first candidate
  (should (eq (liberime-process-event 'space) t))
  (liberime-clear-composition))

(ert-deftest liberime-test-process-event-control-char ()
  "process-event with Control modifier should work."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (let ((c-a (+ ?a #x4000000)))
    ;; Control+a may or may not be handled depending on schema
    (let ((result (liberime-process-event c-a)))
      (should (or (eq result t) (eq result nil))))))

(ert-deftest liberime-test-process-event-return-key ()
  "process-event with 'return should commit composition."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-event ?n)
  (liberime-process-event ?i)
  (let ((input-before (liberime-get-input)))
    (should (string-equal input-before "ni")))
  (liberime-process-event 'return)
  (let ((commit (liberime-get-commit)))
    ;; After return, commit should be consumed or set
    (should (or (null commit) (stringp commit))))
  (liberime-clear-composition))

;; ---------------------------------------------------------------------------
;; kbd-to-key-sequence tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-kbd-to-key-sequence-simple ()
  "kbd-to-key-sequence with simple key should work."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-kbd-to-key-sequence (kbd "a")) "a"))
  (should (string= (liberime-kbd-to-key-sequence "abc") "abc")))

(ert-deftest liberime-test-kbd-to-key-sequence-control ()
  "kbd-to-key-sequence with Control modifier should work."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-kbd-to-key-sequence (kbd "C-a")) "{Control+a}")))

(ert-deftest liberime-test-kbd-to-key-sequence-left ()
  "kbd-to-key-sequence with left arrow should work."
  (liberime-test--skip-unless-rime)
  (should (string= (liberime-kbd-to-key-sequence (kbd "<left>")) "{Left}")))

;; ---------------------------------------------------------------------------
;; process-keys tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-process-keys-simple ()
  "process-keys with simple keys should work."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-keys "abc")
  (should (string-equal (liberime-get-input) "abc"))
  (liberime-clear-composition))

(ert-deftest liberime-test-process-keys-with-modifiers ()
  "process-keys with modifier keys should work."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-keys (kbd "C-a"))
  ;; C-a may or may not add to input depending on schema
  (liberime-clear-composition))

;; ---------------------------------------------------------------------------
;; Output tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-get-input ()
  "liberime-get-input should return input string."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-key ?t)
  (liberime-process-key ?e)
  (liberime-process-key ?s)
  (should (string-equal (liberime-get-input) "tes"))
  (liberime-clear-composition))

(ert-deftest liberime-test-get-context ()
  "liberime-get-context should return context alist."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  (liberime-process-key ?x)
  (let ((context (liberime-get-context)))
    (should (listp context))
    (should (alist-get 'composition context)))
  (liberime-clear-composition))

(ert-deftest liberime-test-get-commit ()
  "liberime-get-commit should return committed text."
  (liberime-test--skip-unless-rime)
  (liberime-clear-composition)
  ;; Type something and commit
  (liberime-process-key ?n)
  (liberime-process-key ?i)
  (liberime-commit-composition)
  (let ((commit (liberime-get-commit)))
    ;; Commit may be nil or a string depending on state
    (should (or (null commit) (stringp commit)))))

;; ---------------------------------------------------------------------------
;; Option tests
;; ---------------------------------------------------------------------------

(defvar liberime-test--saved-options nil
  "Alist of (OPTION . VALUE) snapshots to restore after each option test.")

(defun liberime-test--save-option (option)
  "Snapshot OPTION's current state so it is restored after the test."
  (push (cons option (liberime-get-option option))
        liberime-test--saved-options))

(defun liberime-test--restore-options ()
  "Restore every option snapshotted by `liberime-test--save-option'."
  (dolist (saved liberime-test--saved-options)
    (ignore-errors (liberime-set-option (car saved) (cdr saved))))
  (setq liberime-test--saved-options nil))

(ert-deftest liberime-test-get-option-default-and-boolean ()
  "simplification defaults to off on the fresh test session; results are
always booleans."
  (liberime-test--skip-unless-rime)
  (unwind-protect
      (progn
        (liberime-test--save-option "simplification")
        (liberime-set-option "simplification" nil)
        (should (eq (liberime-get-option "simplification") nil))
        (liberime-test--save-option "ascii_mode")
        (liberime-set-option "ascii_mode" t)
        (should (eq (liberime-get-option "ascii_mode") t)))
    (liberime-test--restore-options)))

(ert-deftest liberime-test-set-option-toggles-and-reads-back ()
  "set-option returns the read-back state; get-option reflects it."
  (liberime-test--skip-unless-rime)
  (unwind-protect
      (progn
        (liberime-test--save-option "simplification")
        (should (eq (liberime-set-option "simplification" t) t))
        (should (eq (liberime-get-option "simplification") t))
        (should (eq (liberime-set-option "simplification" nil) nil))
        (should (eq (liberime-get-option "simplification") nil)))
    (liberime-test--restore-options)))

(ert-deftest liberime-test-set-option-any-option ()
  "The option API is not limited to the get_status booleans."
  (liberime-test--skip-unless-rime)
  (unwind-protect
      (progn
        (liberime-test--save-option "extended_charset")
        (liberime-set-option "extended_charset" nil)
        (should (eq (liberime-set-option "extended_charset" t) t))
        (should (eq (liberime-get-option "extended_charset") t)))
    (liberime-test--restore-options)))

(ert-deftest liberime-test-option-session-isolation ()
  "Setting an option on a created session leaves the default session alone."
  (liberime-test--skip-unless-rime)
  (unwind-protect
      (progn
        (liberime-test--save-option "ascii_mode")
        (liberime-set-option "ascii_mode" nil)
        (let ((session (liberime-session-create "luna_pinyin")))
          (unwind-protect
              (progn
                (should (eq (liberime-set-option "ascii_mode" t session) t))
                (should (eq (liberime-get-option "ascii_mode" session) t))
                (should (eq (liberime-get-option "ascii_mode") nil)))
            (liberime-session-destroy session))))
    (liberime-test--restore-options)))

(ert-deftest liberime-test-option-unknown-option ()
  "Unknown options read as nil; setting one does not error and the value
sticks (librime creates the option)."
  (liberime-test--skip-unless-rime)
  (unwind-protect
      (progn
        (liberime-test--save-option "no_such_option")
        (should (eq (liberime-get-option "no_such_option") nil))
        (should (eq (liberime-set-option "no_such_option" t) t))
        (should (eq (liberime-get-option "no_such_option") t)))
    (liberime-test--restore-options)))

(ert-deftest liberime-test-option-bogus-session-signals ()
  "A bogus SESSION signals a rime error, matching get_status."
  (liberime-test--skip-unless-rime)
  (should-error (liberime-get-option "simplification" 999999))
  (should-error (liberime-set-option "simplification" t 999999)))

;; ---------------------------------------------------------------------------
;; State label tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-get-state-label-simplification ()
  "get-state-label returns the schema's own labels for a switch's states,
and the two states differ."
  (liberime-test--skip-unless-rime)
  (let ((off (liberime-get-state-label "simplification" nil))
        (on (liberime-get-state-label "simplification" t)))
    (should (stringp off))
    (should (stringp on))
    (should (not (string= off on)))))

(ert-deftest liberime-test-get-state-label-schema-without-switch ()
  "A schema that does not define the switch yields a nil label."
  (liberime-test--skip-unless-rime)
  ;; luna_pinyin defines no extended_charset switch
  (should (eq (liberime-get-state-label "extended_charset" t) nil)))

(ert-deftest liberime-test-get-state-label-unknown-option ()
  "Unknown option names yield nil, no error."
  (liberime-test--skip-unless-rime)
  (should (eq (liberime-get-state-label "no_such_option" t) nil)))

(ert-deftest liberime-test-get-state-label-bogus-session-signals ()
  "A bogus SESSION signals a rime error, matching get_option."
  (liberime-test--skip-unless-rime)
  (should-error (liberime-get-state-label "simplification" nil 999999)))

;; ---------------------------------------------------------------------------
;; Interactive option switcher tests
;; ---------------------------------------------------------------------------

(ert-deftest liberime-test-select-option-interactive-is-command ()
  "liberime-select-option-interactive is an interactive command."
  (should (commandp 'liberime-select-option-interactive)))

(ert-deftest liberime-test-select-option-interactive-labels-live ()
  "Candidates use the schema's own labels, off/on only as fallback."
  (liberime-test--skip-unless-rime)
  (let ((candidates
         (catch 'coll
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (_p collection &rest _)
                        (throw 'coll collection))))
             (liberime-select-option-interactive)
             nil))))
    ;; simplification has schema labels (漢字/汉字 under luna_pinyin)
    (should (cl-some (lambda (c)
                       (and (string-prefix-p "simplification " (car c))
                            (not (string-match-p "off -> on\\|on -> off"
                                                 (car c)))))
                     candidates))
    ;; extended_charset has no switch under luna_pinyin -> off/on fallback
    (should (assoc "extended_charset off -> on" candidates))
    ;; one candidate per configured option
    (should (= (length candidates) (length liberime-options)))))

(ert-deftest liberime-test-select-option-interactive-toggles ()
  "Selecting an option flips it via set-option and echoes the transition."
  (liberime-test--skip-unless-rime)
  (liberime-test--save-option "simplification")
  (unwind-protect
      (progn
        (liberime-set-option "simplification" nil)
        (let ((echoed nil))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p collection &rest _)
                       (car (cl-find "simplification" collection
                                     :key #'car :test #'string-prefix-p))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq echoed (apply #'format fmt args)))))
            (liberime-select-option-interactive))
          (should (eq (liberime-get-option "simplification") t))
          (should (string-prefix-p "simplification " echoed))))
    (liberime-test--restore-options)))

;; ---------------------------------------------------------------------------
;; Run tests
;; ---------------------------------------------------------------------------

(defun liberime-test-run ()
  "Run all liberime integration tests."
  (interactive)
  (ert-run-tests-batch-and-exit))

(provide 'liberime-test)

;;; liberime-test.el ends here
