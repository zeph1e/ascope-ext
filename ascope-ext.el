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

(defgroup ascope-ext nil
"An extension for `ascope (http://emacswiki.org/emacs/ascope.el)',
which provides some extra features which are not supported in `ascope':

 - cscope database manipulation
 - keybindings similiar to xcscope
 - more search options
"
 :prefix "ascope-ext-"
 :group 'tools
 :link '(url-link "https://github.com/zeph1e/ascope-ext"))

(require 'ascope)

(defconst proc-createdb "ascope-ext-proc-createdb"
  "The name of worker process which creates `cscope' database.")

(defun start-createdb-worker (dir init)
"Actual worker which create & start db creator process"
)

;; Db creation entry point : checking condition to decide start actual worker or not
(defun ascope-ext-createdb (dir &optional init)
"Creates a cscope.out database in given directory.

DIR is where to create database.
INIT is a flag to decide run `ascope-init' when database is created.
"
  (interactive "DCscope Initial Directory: ")
  (let ((process (get-process proc-createdb)) working-dir do-init (start-worker t))
    (when (processp process)
      (setq working-dir (process-get process 'working-dir))
      (setq do-init (process-get process 'do-init))
      (if (string= working-dir dir)
        (setq start-worker nil)
	(if (yes-or-no-p (concat "Another creating job is running for: " working-dir ". Terminate it? "))
	  (progn
	    (kill-process process)
	    (if (get-buffer "*ascope*") (kill-buffer (get-buffer "*ascope*")))
	    (if (get-process "ascope") (kill-process (get-process "ascope")))
	    (while (processp (get-process proc-createdb)) ;; wait for process termination
	      (sit-for 1)))
	  (setq start-worker nil))))
    (if start-worker (start-createdb-worker dir init))))

;; advice for ascope-init
(defadvice ascope-init (around ascope-init-around (dir))
  (interactive "DCscope Initial Directory: ")
  (if (file-exist-p (concat dir "/cscope.out"))
    ad-do-it
    (ascope-ext-createdb dir t)))
(ad-activate 'ascope-init)

(provide 'ascope-ext)
