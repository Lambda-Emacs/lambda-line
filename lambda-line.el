;;; lambda-line.el --- A custom status line  -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/Lambda-Emacs/lambda-line
;; Keywords: mode-line faces


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lambda-line is a minimal, though opinionated, status-line (i.e. in Emacs the
;; information display either in the mode-line and/or header-line) for use as
;; either header or footer in a buffer. The structure of the status-line takes
;; the following form: [ status | name (primary) tertiary | secondary ]

;; Usage: M-x lambda-line-mode

;;; Code:

(require 'face-remap)
(require 'cl-lib)

;;;; Group

(defgroup lambda-line nil
  "lambda-line group"
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/Lambda-Emacs/lambda-line"))

;;;; Custom Variable Settings

(defcustom lambda-line-window-width-limit 0.25
  "The limit of the window width.
If `window-width' is smaller than the limit, some information won't be
displayed. It can be an integer or a float number. `nil' means no limit."
  :type '(choice integer
                 float
                 (const :tag "Disable" nil))
  :group 'lambda-line)

(defcustom lambda-line-position 'bottom
  "Default modeline position (top or bottom)"
  :type '(choice
          (const :tag "Nil" nil)
          (const :tag "Top"    top)
          (const :tag "Bottom" bottom))
  :group 'lambda-line)

(defcustom lambda-line-prefix t
  "Include a prefix icon to indicate buffer status in the status-line."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'lambda-line)

(defcustom lambda-line-abbrev nil
  "If t then show abbreviated mode symbol in modeline. Default is
nil. To change the values of the major-mode symbols see the value
of lambda-line-abbrev-alist"
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-vc-symbol ""
  "Symbol to use in buffers visiting files under version control"
  :group 'lambda-line
  :type 'string)

;; Visual Bell
(defcustom lambda-line-visual-bell t
  "If t then use lambda-line-visual-bell."
  :group 'lambda-line
  :type 'boolean)

;; Mode line symbols
(defcustom lambda-line-gui-ro-symbol " ⨂"  ;;  ⬤◯⨂
  "Modeline gui read-only symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-gui-mod-symbol " ⬤" ;;  ⨀⬤
  "Modeline gui modified symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-gui-rw-symbol " ◯" ; ◉ ◎ ⬤◯
  "Modeline gui read-write symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-tty-ro-symbol " *"
  "Modeline tty read-only symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-tty-mod-symbol " *"
  "Modeline tty modified symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-tty-rw-symbol " *"
  "Modeline tty read-write symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-truncate-value 30
  "Value of modeline truncate-length function."
  :group 'lambda-line
  :type 'integer)

(defcustom lambda-line-hspace " "
  "Space adjustment for right end of modeline."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-space-top +.35
  "Space adjustment for top of modeline
 Possitive is upwards"
  :type 'float
  :group 'lambda-line)

(defcustom lambda-line-space-bottom -.5
  "Space adjustment for bottom of modeline
 Negative is downwards."
  :type 'float
  :group 'lambda-line)

(defcustom lambda-line-symbol-position .067
  "Space adjustment for bottom of modeline
 Negative is downwards."
  :type 'float
  :group 'lambda-line)

(defcustom lambda-line-syntax t
  "Show flycheck/flymake report in status-line."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-mode-formats
  '(;; with :mode-p first
    (prog-mode              :mode-p lambda-line-prog-mode-p
                            :format lambda-line-prog-mode
                            :on-activate lambda-line-prog-activate
                            :on-deactivate lambda-line-prog-deactivate)
    (mu4e-dashboard-mode    :mode-p lambda-line-mu4e-dashboard-mode-p
                            :format lambda-line-mu4e-dashboard-mode)
    (text-mode              :mode-p lambda-line-text-mode-p
                            :format lambda-line-text-mode)
    (messages-mode          :mode-p lambda-line-messages-mode-p
                            :format lambda-line-messages-mode)
    (message-mode           :mode-p lambda-line-message-mode-p
                            :format lambda-line-message-mode)
    (term-mode              :mode-p lambda-line-term-mode-p
                            :format lambda-line-term-mode)
    (vterm-mode             :mode-p lambda-line-vterm-mode-p
                            :format lambda-line-term-mode)
    (buffer-menu-mode       :mode-p lambda-line-buffer-menu-mode-p
                            :format lambda-line-buffer-menu-mode
                            :on-activate lambda-line-buffer-menu-activate
                            :on-deactivate lambda-line-buffer-menu-deactivate)
    (calendar-mode          :mode-p lambda-line-calendar-mode-p
                            :format lambda-line-calendar-mode
                            :on-activate lambda-line-calendar-activate
                            :on-deactivate lambda-line-calendar-deactivate)
    (completion-list-mode   :mode-p lambda-line-completion-list-mode-p
                            :format lambda-line-completion-list-mode)
    (deft-mode              :mode-p lambda-line-deft-mode-p
      :format lambda-line-deft-mode)
    (doc-view-mode          :mode-p lambda-line-doc-view-mode-p
                            :format lambda-line-doc-view-mode)
    (elfeed-search-mode     :mode-p lambda-line-elfeed-search-mode-p
                            :format lambda-line-elfeed-search-mode
                            :on-activate lambda-line-elfeed-search-activate
                            :on-deactivate lambda-line-elfeed-search-deactivate)
    (elfeed-show-mode       :mode-p lambda-line-elfeed-show-mode-p
                            :format lambda-line-elfeed-show-mode)
    (elpher-mode            :mode-p lambda-line-elpher-mode-p
                            :format lambda-line-elpher-mode
                            :on-activate lambda-line-elpher-activate)
    (info-mode              :mode-p lambda-line-info-mode-p
                            :format lambda-line-info-mode
                            :on-activate lambda-line-info-activate
                            :on-deactivate lambda-line-info-deactivate)
    (mu4e-compose-mode      :mode-p lambda-line-mu4e-compose-mode-p
                            :format lambda-line-mu4e-compose-mode)
    (mu4e-headers-mode      :mode-p lambda-line-mu4e-headers-mode-p
                            :format lambda-line-mu4e-headers-mode)
    (mu4e-loading-mode      :mode-p lambda-line-mu4e-loading-mode-p
                            :format lambda-line-mu4e-loading-mode)
    (mu4e-main-mode         :mode-p lambda-line-mu4e-main-mode-p
                            :format lambda-line-mu4e-main-mode)
    (mu4e-view-mode         :mode-p lambda-line-mu4e-view-mode-p
                            :format lambda-line-mu4e-view-mode)
    (org-agenda-mode        :mode-p lambda-line-org-agenda-mode-p
                            :format lambda-line-org-agenda-mode)
    (org-capture-mode       :mode-p lambda-line-org-capture-mode-p
                            :format lambda-line-org-capture-mode
                            :on-activate lambda-line-org-capture-activate
                            :on-deactivate lambda-line-org-capture-deactivate)
    (org-clock-mode         :mode-p lambda-line-org-clock-mode-p
                            :format lambda-line-org-clock-mode
                            :on-activate lambda-line-org-clock-activate
                            :on-deactivate lambda-line-org-clock-deactivate)
    (pdf-view-mode          :mode-p lambda-line-pdf-view-mode-p
                            :format lambda-line-pdf-view-mode)

    ;; hooks only go last
    (ein-notebook-mode      :on-activate lambda-line-ein-notebook-activate
                            :on-deactivate lambda-line-ein-notebook-deactivate)
    (esh-mode               :on-activate lambda-line-esh-activate
                            :on-deactivate lambda-line-esh-deactivate)
    (ispell-mode            :on-activate lambda-line-ispell-activate
                            :on-deactivate lambda-line-ispell-deactivate)
    (mu4e-mode              :on-activate lambda-line-mu4e-activate
                            :on-deactivate lambda-line-mu4e-deactivate)
    )
  "Modes to be evalued for modeline.
KEY mode name, for reference only. Easier to do lookups and/or replacements.
:MODE-P the function to check if :FORMAT needs to be used, first one wins.
:ON-ACTIVATE and :ON-DEACTIVATE do hook magic on enabling/disabling the mode.
"
  :type '(alist :key-type symbol
                :value-type (plist :key-type (choice (const :mode-p)
                                                     (const :format)
                                                     (const :on-activate)
                                                     (const :on-deactivate))
                                   :value-type function))
  :group 'lambda-line)

(defcustom lambda-line-mode-format-activate-hook nil
  "Add hooks on activation of the mode, for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'lambda-line)

(defcustom lambda-line-mode-format-deactivate-hook nil
  "Remove hooks on de-activation of the mode, for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'lambda-line)

(defcustom lambda-line-default-mode-format 'lambda-line-default-mode
  "Default mode to evaluate if no match could be found in `lambda-lines-mode-formats'"
  :type 'function
  :group 'lambda-line)

;;;; Faces
;;;;; Line Faces

(defface lambda-line-active
  '((t (:inherit (mode-line))))
  "Modeline face for active modeline"
  :group 'lambda-line-active)

(defface lambda-line-inactive
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive window"
  :group 'lambda-line-inactive)

(defface lambda-line-vspace-active
  '((t (:invisible t :height 1.5)))
  "Face for vertical spacer in active line.")

(defface lambda-line-vspace-inactive
  '((t (:invisible t :height 1.5)))
  "Face for vertical spacer in inactive line.")

(defface lambda-line-active-name
  '((t (:inherit (mode-line))))
  "Modeline face for active name element"
  :group 'lambda-line-active)

(defface lambda-line-inactive-name
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive name element"
  :group 'lambda-line-inactive)

(defface lambda-line-active-primary
  '((t (:weight light :inherit (mode-line))))
  "Modeline face for active primary element"
  :group 'lambda-line-active)

(defface lambda-line-inactive-primary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive primary element"
  :group 'lambda-line-inactive)

(defface lambda-line-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'lambda-line-active)

(defface lambda-line-inactive-secondary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive primary element"
  :group 'lambda-line-inactive)


;;;;; Status Bar Faces

;; lambda-line uses a colored symbol to indicate the status of the buffer

(defface lambda-line-active-status-RO
  '((t (:inherit (warning))))
  "Modeline face for active READ-ONLY element"
  :group 'lambda-line-active)

(defface lambda-line-inactive-status-RO
  '((t (:inherit (mode-line-inactive) :foreground "light gray")))
  "Modeline face for inactive READ-ONLY element"
  :group 'lambda-line-inactive)

(defface lambda-line-active-status-RW
  '((t (:inherit (success))))
  "Modeline face for active READ-WRITE element"
  :group 'lambda-line-active)

(defface lambda-line-inactive-status-RW
  '((t (:inherit (mode-line-inactive) :foreground "light gray")))
  "Modeline face for inactive READ-WRITE element"
  :group 'lambda-line-inactive)

(defface lambda-line-active-status-MD
  '((t (:inherit (error))))
  "Modeline face for active MODIFIED element"
  :group 'lambda-line-active)

(defface lambda-line-inactive-status-MD
  '((t (:inherit (mode-line-inactive) :foreground "light gray")))
  "Modeline face for inactive MODIFIED element"
  :group 'lambda-line-inactive)


;;;;; Display Faces

(defface lambda-line-visual-bell '((t (:background "red3")))
  "Face to use for the mode-line when `lambda-line-visual-bell-config' is used."
  :group 'lambda-line)

;;;; Setup Functions

;;;;; Visual bell for mode line

;; See https://github.com/hlissner/emacs-doom-themes for the basic idea

(defun lambda-line-visual-bell-fn ()
  "Blink the status-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((lambda-line--bell-cookie (if (eq lambda-line-position 'bottom)
                                      (face-remap-add-relative 'mode-line 'lambda-line-visual-bell)
                                    (face-remap-add-relative 'header-line 'lambda-line-visual-bell)))
        (force-mode-line-update t))
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update t)))
                    lambda-line--bell-cookie
                    (current-buffer))))

(defun lambda-line-visual-bell-config ()
  "Enable flashing the status-line on error."
  (setq ring-bell-function #'lambda-line-visual-bell-fn
        visible-bell t))

;;;;; Abbreviate Major-Mode
;; Source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings

(defcustom lambda-line-abbrev-alist
  `((dired-mode . "Dir")
    (emacs-lisp-mode . "EL")
    (fundamental-mode . "F")
    (helpful-mode . "")
    (help-mode . "")
    (lisp-interaction-mode . "λ")
    (markdown-mode . "MD")
    (magit-mode . "MG")
    (nxhtml-mode . "NX")
    (prog-mode . "PR")
    (python-mode . "PY")
    (text-mode . "TX"))
  "Alist for `lambda-line--abbrev'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *as substitute for* the original."
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Abbreviation"))
  :group 'lambda-line)

(defun lambda-line--abbrev ()
  (cl-loop for abbrev in lambda-line-abbrev-alist
           do (let* ((mode (car abbrev))
                     (mode-str (cdr abbrev))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

;; Set abbrev (default is nil)
(when lambda-line-abbrev
  (add-hook 'after-change-major-mode-hook #'lambda-line--abbrev))

;;;;; Base Functions
(defun lambda-line-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  lambda-line-user-mode)

(defun lambda-line-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

(defun lambda-line-mode-name ()
  "Return current major mode name"
  (format-mode-line mode-name))

;;;;; String Trim
(defun lambda-line--string-trim-left (string)
  "Remove whitespace at the beginning of STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun lambda-line--string-trim-right (string)
  "Remove whitespace at the end of STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun lambda-line--string-trim (string)
  "Remove whitespace at the beginning and end of STRING."
  (lambda-line--string-trim-left (lambda-line--string-trim-right string)))

;;;;; Branch display
;; -------------------------------------------------------------------
(defun lambda-line-project-name ()
  "return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun lambda-line-vc-project-branch ()
  "If buffer is visiting a file under version control, show project and branch name for file. Otherwise show '-'"
  (let ((backend (vc-backend buffer-file-name)))
    (concat
     (if buffer-file-name
         (if vc-mode
             (let ((project-name (lambda-line-project-name)))
               ;; Project name
               (unless (string= "-" project-name)
                 (concat
                  ;; Divider
                  (propertize " •" 'face `(:inherit fringe))
                  (format " %s" project-name)
                  )))))
     ;; Show branch
     (if vc-mode
         (concat
          lambda-line-vc-symbol (substring-no-properties vc-mode ;    
                                                         (+ (if (eq backend 'Hg) 2 3) 2)))  nil))))

;;;;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(when lambda-line-git-diff-mode-line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    "Show the information of git diff on modeline."
    (setq ad-return-value
	      (concat ad-return-value
		          (let ((plus-minus (vc-git--run-command-string
				                     file "diff" "--numstat" "--")))
		            (if (and plus-minus
		                     (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
		                (concat
                         " "
			             (format "+%s" (match-string 1 plus-minus))
			             (format "-%s" (match-string 2 plus-minus)))
		              (propertize "" 'face '(:weight bold))))))))

;;;;; Dir display


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun lambda-line-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))


;;;;; Flycheck/Flymake Segment
(defvar-local lambda-line--flycheck-text nil)
(defun lambda-line--update-flycheck-segment (&optional status)
  "Update `lambda-line--flycheck-text' against the reported flycheck STATUS."
  (setq lambda-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "⚑ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'error
                                                 'warning))))
                       (propertize "✔ Good " 'face 'success)))
          ('running (propertize "Δ Checking " 'face 'info))
          ('errored (propertize "✖ Error " 'face 'error))
          ('interrupted (propertize "⏸ Paused " 'face 'lambda-line-inactive))
          ('no-checker ""))))

(defun lambda-line-check-syntax ()
  "Displays syntax-checking information from flymake/flycheck in
the mode-line (if available)."
  (if (and (boundp 'flymake-mode) flymake-mode)
      (concat (lambda-line--string-trim (format-mode-line
                                         flymake--mode-line-format)) " ") lambda-line--flycheck-text))

;;;;; Vertical Spacer

(defvar lambda-line-vspace "⁣"
  "Use an invisible character to generate vertical space (i.e. padding) in the status-line.")

;;;;; Mode line status
;; ---------------------------------------------------------------------
(defun lambda-line-status ()
  "Return buffer status.
Default symbols are set for both GUI (using UTF-8 symbols) and
tty (using regular ASCII characters)."
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    ;; Use status letters for TTY display
    (cond
     (modified
      (if (display-graphic-p)
          lambda-line-gui-mod-symbol
        lambda-line-tty-mod-symbol))
     (read-only
      (if (display-graphic-p)
          lambda-line-gui-ro-symbol
        lambda-line-tty-ro-symbol))
     (t (if (display-graphic-p)
            lambda-line-gui-rw-symbol
          lambda-line-tty-rw-symbol)))))


;;;; Compose mode line
;; -------------------------------------------------------------------

(defun lambda-line-compose (status name primary tertiary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (active        (eq window lambda-line--selected-window))
         (read-only     buffer-read-only)
         (modified      (and buffer-file-name (buffer-modified-p)))
         (read-write    (not (or (buffer-modified-p) buffer-read-only)))
         (prefix (if (display-graphic-p)
                     (cond (read-only
                            (propertize (if (window-dedicated-p)" –– " lambda-line-gui-ro-symbol)
                                        'face (if active
                                                  'lambda-line-active-status-RO
                                                'lambda-line-inactive-status-RO)
                                        'display `(raise ,lambda-line-symbol-position)))
                           (modified
                            (propertize (if (window-dedicated-p)" –– " lambda-line-gui-mod-symbol)
                                        'face (if active
                                                  'lambda-line-active-status-MD
                                                'lambda-line-inactive-status-MD)
                                        'display `(raise ,lambda-line-symbol-position)))
                           (read-write
                            (propertize (if (window-dedicated-p) " –– " lambda-line-gui-rw-symbol)
                                        'face (if active
                                                  'lambda-line-active-status-RW
                                                'lambda-line-inactive-status-RW)
                                        'display `(raise ,lambda-line-symbol-position)))
                           (t
                            (propertize status
                                        'face (if active
                                                  'lambda-line-active-status-MD
                                                'lambda-line-inactive-status-MD)
                                        'display `(raise ,lambda-line-symbol-position))))
                   ;; TTY displays
                   (cond (read-only
                          (propertize
                           (if (window-dedicated-p) " -- " lambda-line-tty-ro-symbol)
                           'face (if active
                                     'lambda-line-active-status-RO
                                   'lambda-line-inactive-status-RO)))
                         (modified
                          (propertize
                           (if (window-dedicated-p) " -- " lambda-line-tty-mod-symbol)
                           'face (if active
                                     'lambda-line-active-status-MD
                                   'lambda-line-inactive-status-MD)))
                         (read-write
                          (propertize
                           (if (window-dedicated-p) " -- " lambda-line-tty-rw-symbol)
                           'face (if active
                                     'lambda-line-active-status-RW
                                   'lambda-line-inactive-status-RW)))
                         (t
                          (propertize status
                                      'face (if active
                                                'lambda-line-active-status-MD
                                              'lambda-line-inactive-status-MD))))))

         (left (concat
                ;; add invisible char for extra vertical padding
                (propertize lambda-line-vspace 'face (if active 'lambda-line-vspace-active
                                                       'lambda-line-vspace-inactive))
                (propertize " "  'face (if active 'lambda-line-active
                                         'lambda-line-inactive)
                            'display `(raise ,lambda-line-space-top))
                (propertize name 'face (if active 'lambda-line-active-name
                                         'lambda-line-inactive-name))
                (propertize " "  'face (if active 'lambda-line-active
                                         'lambda-line-inactive)
                            'display `(raise ,lambda-line-space-bottom))
                (propertize primary 'face (if active 'lambda-line-active-primary
                                            'lambda-line-inactive-primary))))
         (right (concat tertiary " " secondary lambda-line-hspace))

         (available-width (- (window-total-width)
                             (length prefix) (length left) (length right)
                             (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat (if lambda-line-prefix prefix nil)
            left
            (propertize (make-string available-width ?\ )
                        'face (if active 'lambda-line-active
                                'lambda-line-inactive))
            right)))

;;;; Default display

(defun lambda-line-default-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (lambda-line-mode-name))
        (branch      (lambda-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (lambda-line-compose (lambda-line-status)
                         (lambda-line-truncate buffer-name lambda-line-truncate-value)
                         (concat "(" mode-name
                                 (when branch
                                   branch)
                                 ")")
                         nil
                         (concat
                          ;; Narrowed buffer
                          (if (buffer-narrowed-p)
                              (concat
                               (propertize "⇥ "  'face `(:inherit lambda-line-inactive-secondary))
                               position " ")
                            position)))))


;;;; Mode Functions
;;;;; Prog Mode
;; ---------------------------------------------------------------------
(defun lambda-line-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun lambda-line-prog-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (lambda-line-mode-name))
        (branch      (lambda-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (lambda-line-compose (lambda-line-status)
                         (lambda-line-truncate buffer-name lambda-line-truncate-value)
                         (concat "(" mode-name
                                 (when branch
                                   branch)
                                 ")")
                         (lambda-line-check-syntax)
                         (concat
                          ;; Narrowed buffer
                          (when (buffer-narrowed-p)
                            (propertize "⇥ "  'face `(:inherit lambda-line-inactive-secondary)))
                          (if lambda-line-syntax
                              (if (or (boundp 'flycheck-mode)
                                      (boundp 'flymake-mode))
                                  (concat position lambda-line-hspace)
                                position)
                            position)))))

(defun lambda-line-prog-activate ()
  ;; Setup flycheck hooks
  (add-hook 'flycheck-status-changed-functions #'lambda-line--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'lambda-line--update-flycheck-segment)
  )

(defun lambda-line-prog-deactivate ()
  ;; Remove flycheck hooks
  (remove-hook 'flycheck-status-changed-functions #'lambda-line--update-flycheck-segment)
  (remove-hook 'flycheck-mode-hook #'lambda-line--update-flycheck-segment)
  )

;;;;; Text Mode

(defun lambda-line-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun lambda-line-text-mode ()
  (lambda-line-default-mode))

;;;;; Info Display
;; ---------------------------------------------------------------------
(defun lambda-line-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	    (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	    line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			                     crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	         (if (not (equal node "Top")) node
	           (format "%s"
		               (if (stringp Info-current-file)
			               (file-name-sans-extension
			                (file-name-nondirectory Info-current-file))
			             Info-current-file)))))
	    (setq line (concat line (if (null line) "" " > ")
                           (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun lambda-line-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun lambda-line-info-mode ()
  (lambda-line-compose ""
                       "INFO"
                       (concat "("
                               (lambda-line-info-breadcrumbs)
                               ")")
                       nil
                       ""))

(defun lambda-line-info-activate ()
  (if (eq lambda-line-position 'top)
      (setq Info-use-header-line nil)))

(defun lambda-line-info-deactivate ()
  (custom-reevaluate-setting 'Info-use-header-line))

;;;; Term & Vterm
;; ---------------------------------------------------------------------
;; term
(defun lambda-line-term-mode-p ()
  (derived-mode-p 'term-mode))

;; vterm
(defun lambda-line-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun lambda-line-term-mode ()
  (lambda-line-compose " >_ "
                       "Terminal"
                       (concat "(" (file-name-nondirectory shell-file-name) ")")
                       nil
                       (lambda-line-shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------

(defun lambda-line-get-ssh-host (_str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun lambda-line-ssh-mode ()
  (lambda-line-compose " >_ "
                       "Terminal"
                       (concat "(" (lambda-line-get-ssh-host default-directory) ")")
                       nil
                       (lambda-line-shorten-directory (car (last (split-string default-directory ":"))) 32)))

;;;; Messages Buffer Mode
;; ---------------------------------------------------------------------
(defun lambda-line-messages-mode-p ()
  (derived-mode-p 'messages-buffer-mode))

(defun lambda-line-messages-mode ()
  (lambda-line-compose (lambda-line-status)
                       "*Messages*" "" nil ""))

;;;; Message Mode
;; ---------------------------------------------------------------------
(defun lambda-line-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun lambda-line-message-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Message" "(Draft)" nil ""))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun lambda-line-doc-view-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun lambda-line-doc-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	    (mode-name   (lambda-line-mode-name))
	    (branch      (lambda-line-vc-project-branch))
	    (page-number (concat
		              (number-to-string (doc-view-current-page)) "/"
		              (or (ignore-errors
			                (number-to-string (doc-view-last-page-number)))
			              "???"))))
    (lambda-line-compose
     (lambda-line-status)
     buffer-name
     (concat "(" mode-name
             branch
	         ")" )
     nil
     page-number)))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun lambda-line-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (require 'pdf-view))

(defun lambda-line-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	    (mode-name   (lambda-line-mode-name))
	    (branch      (lambda-line-vc-project-branch))
	    (page-number (concat
		              (number-to-string (eval `(pdf-view-current-page))) "/"
		              (or (ignore-errors
			                (number-to-string (pdf-cache-number-of-pages)))
			              "???"))))
    (lambda-line-compose
     (lambda-line-status)
     buffer-name
     (concat "(" mode-name
             branch
	         ")" )
     nil
     (concat page-number " "))))

;;;; MenuMode

(defun lambda-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun lambda-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (lambda-line-mode-name))
        (position    (format-mode-line "%l:%c:%o")))

    (lambda-line-compose (lambda-line-status)
                         buffer-name "" nil (concat position lambda-line-hspace))))


;;;; Completion
;; ---------------------------------------------------------------------
(defun lambda-line-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun lambda-line-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (lambda-line-mode-name))
        (position    (format-mode-line "%l:%c:%o")))

    (lambda-line-compose (lambda-line-status)
                         buffer-name "" nil (concat position lambda-line-hspace))))

;;;; Deft Mode

(with-eval-after-load 'deft
  (defun lambda-line--deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun lambda-line-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun lambda-line-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (lambda-line-compose prefix primary filter nil matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun lambda-line-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun lambda-line-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun lambda-line-calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default)))))

(defun lambda-line-calendar-activate ()
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'lambda-line-calendar-setup-header)))

(defun lambda-line-calendar-deactivate ()
  (remove-hook 'calendar-initial-window-hook
               #'lambda-line-calendar-setup-header))

;;;; Org Capture
;; ---------------------------------------------------------------------
(defun lambda-line-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun lambda-line-org-capture-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Capture"
                       (concat "(" (org-capture-get :description) ")")
                       nil
                       ""))

(defun lambda-line-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

(defun lambda-line-org-capture-activate ()
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'lambda-line-org-capture-turn-off-header-line)))

(defun lambda-line-org-capture-deactivate ()
  (remove-hook 'org-capture-mode-hook
               #'lambda-line-org-capture-turn-off-header-line))


;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun lambda-line-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun lambda-line-org-agenda-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Agenda"
                       ""
                       nil
                       (concat (propertize "◴"
                                           'face 'lambda-line-active-secondary
                                           'display '(raise 0.06))
                               (format-time-string "%H:%M "))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(defun lambda-line-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (stringp org-mode-line-string)))

(defun lambda-line-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (lambda-line-mode-name))
        (branch      (lambda-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (lambda-line-compose (lambda-line-status)
                         buffer-name
                         (concat "(" mode-name
                                 (when branch
                                   branch)
                                 ")" )
                         (concat
                          ;; Narrowed buffer
                          (when (buffer-narrowed-p)
                            (propertize "⇥ "  'face `(:inherit lambda-line-inactive-secondary)))
                          org-mode-line-string
                          " "
                          nil
                          position
                          lambda-line-hspace))))

(defun lambda-line-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun lambda-line-org-clock-activate ()
  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'lambda-line-org-clock-out)))

(defun lambda-line-org-clock-deactivate ()
  (remove-hook 'org-clock-out-hook
               #'lambda-line-org-clock-out))

;;;; Elfeed
;; ---------------------------------------------------------------------
(defun lambda-line-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun lambda-line-elfeed-search-mode ()
  (let* ((prefix "NEWS")
         (no-database (zerop (elfeed-db-last-update)))
         (update      (> (elfeed-queue-count-total) 0))

         (name  (cond (no-database "No database")
                      (update      "Update:")
                      (t           "Search:")))
         (primary (cond  (no-database "")
                         (update
                          (let ((total (elfeed-queue-count-total))
                                (in-process (elfeed-queue-count-active)))
                            (format "%d jobs pending, %d active"
                                    (- total in-process) in-process)))
                         (t  (let* ((db-time (seconds-to-time (elfeed-db-last-update)))
                                    (unread ))
                               (cond (elfeed-search-filter-active "")
                                     ((string-match-p "[^ ]" elfeed-search-filter)
                                      elfeed-search-filter)
                                     (""))))))
         (secondary (cond
                     ((zerop (elfeed-db-last-update)) "")
                     ((> (elfeed-queue-count-total) 0) "")
                     (t (elfeed-search--count-unread)))))
    (lambda-line-compose nil name primary nil secondary)))

;; Elfeed uses header-line, we need to tell it to use our own format
(defun lambda-line-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

(defun lambda-line-elfeed-search-activate ()
  (with-eval-after-load 'elfeed
    (if (eq lambda-line-position 'top)
        (setq elfeed-search-header-function #'lambda-line-elfeed-setup-header))))

(defun lambda-line-elfeed-search-deactivate ()
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header)))

;; ---------------------------------------------------------------------
(defun lambda-line-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun lambda-line-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (lambda-line-compose nil
                         title
                         ;; (nano-modeline-truncate title 40)
                         (concat "(" tags-str ")")
                         feed-title)))


;;;; Mu4e

(defun lambda-line-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun lambda-line-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))

(defun lambda-line-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (string> mu4e-mu-version "1.6.5")
      mu4e--server-props
    mu4e~server-props))

(defun lambda-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'lambda-line)))

(defun lambda-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'lambda-line))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun lambda-line-mu4e-dashboard-mode ()
  (lambda-line-compose (lambda-line-status)
                       (format "%d messages"
                               (plist-get (lambda-line-mu4e-server-props) :doccount))
                       ""
                       nil
                       " "))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))


(defun lambda-line-mu4e-loading-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Loading..."
                       (lambda-line-mu4e-context)
                       nil
                       (format-time-string "%A %d %B %Y, %H:%M ")))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun lambda-line-mu4e-main-mode ()
  (lambda-line-compose (lambda-line-status)
                       (lambda-line-mu4e-context)
                       ""
                       nil
                       (format-time-string "%A %d %B %Y, %H:%M ")))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-compose-mode-p ()
  (derived-mode-p 'mu4e-compose-mode))

(defun lambda-line-mu4e-compose-mode ()
  (lambda-line-compose (lambda-line-status)
                       (format-mode-line "%b")
                       ""
                       nil
                       (format "[%s] "
                               (lambda-line-mu4e-quote
                                (mu4e-context-name (mu4e-context-current))))))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-quote (str)
  (if (version< "1.6.5" mu4e-mu-version)
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun lambda-line-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun lambda-line-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (lambda-line-compose (lambda-line-status)
                         "Search:"
                         (or (lambda-line-mu4e-quote
                              (lambda-line-mu4e-last-query)) "")
                         nil
                         (format "[%s] "
                                 (lambda-line-mu4e-quote
                                  (mu4e-context-name (mu4e-context-current)))))))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun lambda-line-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date    (mu4e-message-field msg :date)))
    (lambda-line-compose (lambda-line-status)
                         (or from "")
                         (concat "(" (lambda-line-truncate (or subject "") 50 "…") ")")
                         nil
                         (concat (or (format-time-string mu4e-headers-date-format date) "") " "))))

(defun lambda-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'lambda-line)))

(defun lambda-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'lambda-line))

;;;; Ein

(defun lambda-line-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (lambda-line-compose (if (ein:notebook-modified-p) "MD" "RW")
                         buffer-name
                         ""
                         nil
                         (ein:header-line))))

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the lambda-line function to set
;; the header format in a notebook buffer. Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(defun lambda-line-ein-notebook-activate ()
  (with-eval-after-load 'ein
    (if (eq lambda-line-position 'top)
        (setq ein:header-line-format '((:eval (lambda-line-ein-notebook-mode)))))))

(defun lambda-line-ein-notebook-deactivate ()
  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line)))))


;;;; Buffer Menu Mode
;; ---------------------------------------------------------------------
(defun lambda-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun lambda-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (lambda-line-mode-name))
        (position    (format-mode-line "%l:%c")))

    (lambda-line-compose nil
                         buffer-name "" nil position)))

;;(defun buffer-menu-mode-header-line ()
;;  (face-remap-add-relative
;;   'header-line `(:background ,(face-background 'nano-subtle))))
;;(add-hook 'Buffer-menu-mode-hook
;;          #'buffer-menu-mode-header-line)

(defun lambda-line-buffer-menu-activate ()
  (if (eq lambda-line-position 'top)
      (setq Buffer-menu-use-header-line nil)))

(defun lambda-line-buffer-menu-deactivate ()
  (custom-reevaluate-setting 'Buffer-menu-use-header-line))

;;;; Elpher Mode
;; ---------------------------------------------------------------------
(defun lambda-line-elpher-mode-p ()
  (derived-mode-p 'elpher-mode))

(defun lambda-line-elpher-mode ()
  (let* ((display-string (elpher-page-display-string elpher-current-page))
         (sanitized-display-string (replace-regexp-in-string "%" "%%" display-string))
         (address (elpher-page-address elpher-current-page))
         (tls-string (if (and (not (elpher-address-about-p address))
                              (member (elpher-address-protocol address)
                                      '("gophers" "gemini")))
                         "(TLS encryption)"
                       "")))
    (lambda-line-compose nil
                         sanitized-display-string
                         tls-string
                         nil
                         "")))

(defun lambda-line-elpher-activate ()
  (with-eval-after-load 'elpher
    (setq elpher-use-header nil)))

;;;; Esh Mode
;; ---------------------------------------------------------------------
(defun lambda-line-esh-activate ()
  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil)))

(defun lambda-line-esh-deactivate ()
  (custom-reevaluate-setting 'eshell-status-in-mode-line))

;;;; Ispell Mode
;; ---------------------------------------------------------------------
(defun lambda-line-enlarge-ispell-choices-buffer (buffer)
  (when (string= (buffer-name buffer) "*Choices*")
    (with-current-buffer buffer
      ;; (enlarge-window +2)
      (setq-local header-line-format nil)
      (setq-local mode-line-format nil))))

(defun lambda-line-ispell-activate ()
  (with-eval-after-load 'ispell
    (advice-add #'ispell-display-buffer :after
                #'lambda-line-enlarge-ispell-choices-buffer)))

(defun lambda-line-ispell-deactivate ()
  (advice-remove #'ispell-display-buffer
                 #'lambda-line-enlarge-ispell-choices-buffer))

;;;; Setup Lambda-line
;; ---------------------------------------------------------------------
(defun lambda-line-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))

;; ---------------------------------------------------------------------
(defvar lambda-line--saved-mode-line-format nil)
(defvar lambda-line--saved-header-line-format nil)
(defvar lambda-line--selected-window nil)

(defun lambda-line--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq lambda-line--selected-window (selected-window)))

(defun lambda-line ()
  "Build and set the modeline."
  (let* ((format
          '((:eval
             (funcall
              (or (catch 'found
                    (dolist (elt lambda-line-mode-formats)
                      (let* ((config (cdr elt))
                             (mode-p (plist-get config :mode-p))
                             (format (plist-get config :format)))
                        (when mode-p
                          (when (funcall mode-p)
                            (throw 'found format))))))
                  lambda-line-default-mode-format))))))
    (if (eq lambda-line-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))


(defun lambda-line-update-windows ()
  "Hide the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (setq mode-line-format
                  (cond ((one-window-p t) (list ""))
                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                        ((not (window-in-direction 'below)) (list ""))
                        (t nil))))))))



(defun lambda-line-mode--activate ()
  "Activate lambda-line."

  ;; Save current mode-line and header-line
  (unless lambda-line--saved-mode-line-format
    (setq lambda-line--saved-mode-line-format mode-line-format)
    (setq lambda-line--saved-header-line-format header-line-format))

  (dolist (elt lambda-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-activate)))
      (when fn (funcall fn))))

  (run-hooks 'lambda-line-mode-format-activate-hook)

  ;; Update selected window
  (lambda-line--update-selected-window)
  ;; (setq lambda-line--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (lambda-line)

  ;; Use lambda-line-visual-bell when var is set to t
  (when lambda-line-visual-bell
    (lambda-line-visual-bell-config))

  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'lambda-line--update-selected-window)

  ;; This hooks hide the modeline for windows having a window below them
  ;; Disabled for the time being,
  ;;  -> see https://github.com/rougier/nano-modeline/issues/24
  ;; (add-hook 'window-configuration-change-hook #'lambda-line-update-windows)

  (force-mode-line-update t))

;; Deactivate status-line
(defun lambda-line-mode--deactivate ()
  "Deactivate lambda-line and restore default mode-line."

  (dolist (elt lambda-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-deactivate)))
      (when fn (funcall fn))))

  (run-hooks 'lambda-line-mode-format-deactivate-hook)

  (remove-hook 'post-command-hook
               #'lambda-line--update-selected-window)
  (remove-hook 'window-configuration-change-hook
               #'lambda-line-update-windows)

  ;; Deactivate lambda-line-visual-bell
  (setq lambda-line-visual-bell nil)

  (setq         mode-line-format lambda-line--saved-mode-line-format)
  (setq-default mode-line-format lambda-line--saved-mode-line-format)
  (setq         header-line-format lambda-line--saved-header-line-format)
  (setq-default header-line-format lambda-line--saved-header-line-format))

;;;; Lambda-line minor mode

;; Store the default mode-line format
(defvar lambda-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode lambda-line-mode
  "Toggle lambda-line on or off."
  :group 'lambda-line
  :global t
  :lighter nil

  (if lambda-line-mode
      (lambda-line-mode--activate)
    (lambda-line-mode--deactivate))

  ;; Run any registered hooks
  (run-hooks 'lambda-line-mode-hook))

;;; Provide:
  (provide 'lambda-line)

;;; lambda-line.el ends here
