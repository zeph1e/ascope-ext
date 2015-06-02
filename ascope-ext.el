;; Copyright (c) 2015 Yunsik Jang <doomsday@kldp.org>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; ascope-ext (https://github.com/zeph1e/ascope-ext)
;;
;; An extension for ascope (http://emacswiki.org/emacs/ascope.el),
;; which provides some extra features which are not supported in ascope:
;;
;;  - cscope database manipulation
;;  - a minor mode, ascope-mode for key bindings
;;  - more search options (egrep-pattern, filename, assignment-to-the-symbol)
;;
;; Keybindings:
;;
;;  C-c s /  Set initial directory (source root).
;;  C-c s s  Find this symbol.
;;  C-c s d  Find global definition.
;;  C-c s g  Find global definition.
;;  C-c s C  Find called functions.
;;  C-c s c  Find functions calling this function.
;;  C-c s t  Find this text string.
;;  C-c s e  Find this egrep pattern.
;;  C-c s i  Find files including file.
;;  C-c s f  Find this file.
;;  C-c s =  Find assignments to this symbol.
;;  C-c s a  Find assignments to this symbol.
;;  C-c s A  Find all symbol assignments.


(defgroup ascope-ext nil
"An extension for `ascope (http://emacswiki.org/emacs/ascope.el)',
which provides some extra features which are not supported in `ascope':

 - cscope database manipulation
 - a minor mode for key bindings
 - more search options
"
 :prefix "ascope-"
 :group 'tools
 :link '(url-link "https://github.com/zeph1e/ascope-ext"))

(require 'ascope)

(defconst ascope-create-database-proc "ascope-create-database"
  "The name of worker process which creates `cscope' database.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search options which are not supported in ascope
(defun ascope-find-this-egrep-pattern (symbol)
"Locate where this egrep pattern matches."
  (interactive (ascope-interactive "Find this egrep pattern: "))
  (setq query-command (concat "6" symbol "\n"))
  (ring-insert ascope-marker-ring (point-marker))
  (setq ascope-action-message (format "Finding this egrep pattern: %s" symbol))
  (ascope-query query-command))

(defun ascope-find-this-file (symbol)
"Find files which have given string in their name."
  (interactive (ascope-interactive "Find this file: "))
  (setq query-command (concat "7" symbol "\n"))
  (ring-insert ascope-marker-ring (point-marker))
  (setq ascope-action-message (format "Finding this file: %s" symbol))
  (ascope-query query-command))

(defun ascope-find-assignments-to-this-symbol (symbol)
"Locate assignments to a symbol in the source."
  (interactive (ascope-interactive "Find assignments to this symbol: "))
  (setq query-command (concat "9" symbol "\n"))
  (ring-insert ascope-marker-ring (point-marker))
  (setq ascope-action-message (format "Finding assignments to this symbol: %s" symbol))
  (ascope-query query-command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cscope.out db creation
(defun ascope-create-database-sentinel (process event)
  (let ((dir (process-get process 'working-dir))
	(init (process-get process 'do-init)))
    (cond
      ((and (string-match "finished" event) (file-exists-p (concat dir "/cscope.out")))
        (unless init (message "ascope: cscope.out created: %S" dir))
        (ascope-init dir)))))

(defun ascope-create-database (dir &optional init)
"Create a cscope.out database in given directory.

DIR is where to create database.
INIT is a flag to decide run `ascope-init' when database is created.
"
  (interactive "DCscope Initial Directory: ")
  (let ((process (get-process ascope-create-database-proc)) working-dir do-init (start-worker t))
    (when (processp process)
      (setq working-dir (process-get process 'working-dir))
      (setq do-init (process-get process 'do-init))
      (if (string= working-dir dir)
        (setq start-worker nil)
	(if (yes-or-no-p
	      (concat "ascope: terminate running job for " dir "? "))
	  (progn
	    (kill-process process)
	    (if (get-buffer "*ascope*") (kill-buffer (get-buffer "*ascope*")))
	    (if (get-process "ascope") (kill-process (get-process "ascope")))
	    (while (processp (get-process ascope-create-database-proc)) ; wait for process termination
	      (sit-for 1)))
	  (setq start-worker nil))))
    (if start-worker
      (unless (and init (not (file-exists-p (concat dir "/cscope.out")))
		   (not (y-or-n-p "ascope: no cscope.out file here. create? ")))
	(progn
	  (setq default-directory dir)
	  (message "ascope: creating cscope.out in %S..." dir)
	  (start-process ascope-create-database-proc nil "cscope" "-bR")
	  (setq process (get-process ascope-create-database-proc))
	  (process-put process 'working-dir dir)
	  (process-put process 'do-init init)
	  (set-process-sentinel process 'ascope-create-database-sentinel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advices which overrides functions in ascope
;;;###autoload
(defadvice ascope-init (around ascope-init-around (dir))
  (interactive "DCscope Initial Directory: ")
  (if (file-exists-p (concat dir "/cscope.out"))
    (progn
      ad-do-it
      (process-put (get-process "ascope") 'initial-directory dir))
    (ascope-create-database dir t)))

(defadvice ascope-query (around ascope-query-around (command))
  (if (processp (get-process "ascope"))
    ad-do-it
    (message "ascope: do ascope-init first.")))

(ad-activate 'ascope-init)
(ad-activate 'ascope-query)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor-mode definitions
(defvar ascope-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s s") 'ascope-find-this-symbol)
    (define-key map (kbd "C-c s d") 'ascope-find-global-definition)
    (define-key map (kbd "C-c s g") 'ascope-find-global-definition)
    (define-key map (kbd "C-c s C") 'ascope-find-called-functions)
    (define-key map (kbd "C-c s c") 'ascope-find-functions-calling-this-function)
    (define-key map (kbd "C-c s t") 'ascope-find-this-text-string)
    (define-key map (kbd "C-c s e") 'ascope-find-this-egrep-pattern)
    (define-key map (kbd "C-c s i") 'ascope-find-files-including-file)
    (define-key map (kbd "C-c s f") 'ascope-find-this-file)
    (define-key map (kbd "C-c s =") 'ascope-find-assignments-to-this-symbol)
    (define-key map (kbd "C-c s a") 'ascope-find-assignments-to-this-symbol)
    (define-key map (kbd "C-c s A") 'ascope-all-symbol-assignments)
    (define-key map (kbd "C-c s /") 'ascope-init)
    map)
  "The ascope keymap")

;;;###autoload
(define-minor-mode ascope-mode
"This ascope minor mode supports hotkeys to search cscope database.

Key bindings:
\\{ascope-mode-keymap}"
  nil nil ascope-mode-keymap
  :group 'tools)

;;;###autoload
(defun ascope-install-hooks ()
  (interactive)
  (add-hook 'c-mode-hook 'ascope-mode)
  (add-hook 'c++-mode-hook 'ascope-mode)
  (add-hook 'cc-mode-hook 'ascope-mode)
  (add-hook 'idl-mode-hook 'ascope-mode)
  (add-hook 'dired-mode-hook 'ascope-mode))


(provide 'ascope-ext)
