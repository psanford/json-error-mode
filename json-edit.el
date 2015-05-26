;;; json-edit.el --- Good JSON editing mode

;; Author:  Peter Sanford (psanford@petersdanceparty.com)
;; Keywords:  json

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This is a json editing mode that supports full json parsing. With
;; a full parser we are able to provide accurate syntax-error checking.

;;; Code:

(defvar json-edit-EOF-CHAR -1)

;; Continue.
(defvar json-edit-scan-continue 0)
(defvar json-edit-scan-begin-literal 1)
(defvar json-edit-scan-begin-object 2)
(defvar json-edit-scan-object-key 3)
(defvar json-edit-scan-object-value 4)
(defvar json-edit-scan-end-object 5)
(defvar json-edit-scan-begin-array 6)
(defvar json-edit-scan-array-value 7)
(defvar json-edit-scan-end-array 8)
(defvar json-edit-scan-skip-space 9)
;; Stop.
(defvar json-edit-scan-end 10)
(defvar json-edit-scan-error 11)


(make-local-variable 'json-edit-cursor) ; current position
(make-local-variable 'json-edit-step) ; next parse function
(make-local-variable 'json-edit-parse-state) ; Stack of what we are in the middle of
(make-local-variable 'json-edit-error) ; error that happened, if any
(make-local-variable 'json-edit-lineno) ; line number
(make-local-variable 'json-edit-line-offset) ; char offset in line


(defun json-edit-init-scanner (&optional buf)
  (save-excursion
    (when buf (set-buffer buf))
    (setq
     json-edit-cursor (point-min)
     json-edit-step 'json-edit-state-begin-value
     json-edit-parse-state '()
     json-edit-error nil
     json-edit-lineno 1
     json-edit-line-offset 0)))

(defun json-edit-scan-all ()
  (let (c state)
    (catch 'done
      (while t
        (setq c (json-edit-next-char))
        (when (eq c json-edit-EOF-CHAR)
          (throw 'done nil))
        (setq state (funcall json-edit-step c))
        (when (>= state json-edit-scan-skip-space)
          (when (= state json-edit-scan-error)
            (message "error!! %s %d %d" json-edit-error json-edit-lineno json-edit-line-offset)
            (throw 'done nil)))))
    ))

(defun json-edit-next-char ()
  (let (c)
    (if (>= json-edit-cursor (point-max))
        (setq c json-edit-EOF-CHAR)
      (setq c (char-after json-edit-cursor))
      (when (= c ?\n)
        (setq json-edit-lineno (+ 1 json-edit-lineno))
        (setq json-edit-line-offset -1))
      (setq json-edit-cursor (+ 1 json-edit-cursor))
      (setq json-edit-line-offset (+ 1 json-edit-line-offset)))
    c))

(defun json-edit-state-error (c)
  "state after reaching a syntax error"
  json-edit-scan-error)

(defun json-edit-set-error (c context)
  "record error and switch to error state"
  (setq json-edit-step 'json-edit-state-error
        json-edit-error (format "invalid character '%c' %s" c context))
  json-edit-scan-error)

(defsubst json-edit-is-space (c)
  "returns t if character is whitespace"
  (case c
    (?\s t)
    (?\t t)
    (?\r t)
    (?\n t)
    (t nil)))

(defun json-edit-state-begin-value-or-empty (c)
  "state after reading `[`"
  (cond
   ((json-edit-is-space c)
    json-edit-scan-skip-space)
   ((= c ?\])
    (json-edit-state-end-value c))
   (t
    (json-edit-state-begin-value c))))

(defun json-edit-state-begin-value (c)
  "State at the beginning of the input."
  (case c
    (?\s json-edit-scan-skip-space)
    (?\t json-edit-scan-skip-space)
    (?\r json-edit-scan-skip-space)
    (?\n json-edit-scan-skip-space)
    (?{  (progn
           (setq json-edit-step 'json-edit-state-begin-string-or-empty)
           (push 'json-edit-parse-object-key json-edit-parse-state)
           json-edit-scan-begin-object))
    (?\[ (progn
           (setq json-edit-step 'json-edit-state-begin-value-or-empty)
           (push 'json-edit-parse-array-value json-edit-parse-state)
           json-edit-scan-begin-array))
    (?\" (progn
           (setq json-edit-step 'json-edit-state-in-string)
           json-edit-scan-begin-literal))
    (?-  (progn
           (setq json-edit-step 'json-edit-state-neg)
           json-edit-scan-begin-literal))
    (?0  (progn                         ; beginning of 0.123
           (setq json-edit-step 'json-edit-state-0)
           json-edit-scan-begin-literal))
    (?t  (progn                         ; beginning of true
           (setq json-edit-step 'json-edit-state-t)
           json-edit-scan-begin-literal))
    (?f  (progn                         ; beginning of false
           (setq json-edit-step 'json-edit-state-f)
           json-edit-scan-begin-literal))
    (?n  (progn                         ; beginning of null
           (setq json-edit-step 'json-edit-state-n)
           json-edit-scan-begin-literal))
    (t
     (if (and (<= ?1 c) (<= c ?9))
         (progn                         ; beginning of 1234.5
           (setq json-edit-step 'json-edit-state-1)
           json-edit-scan-begin-literal)
                                        ; else error
       (json-edit-set-error c "looking for beginning of value")))))


(defun json-edit-state-begin-string-or-empty (c)
  "state after reading ?{"
  (cond
   ((json-edit-is-space c)
    json-edit-scan-skip-space)
   ((= c ?})
    (pop json-edit-parse-state)
    (push 'json-edit-parse-object-value json-edit-parse-state)
    (json-edit-state-end-value c))
   (t
    (json-edit-state-begin-string c))))

(defun json-edit-state-begin-string (c)
  "state after reading `{\"key\": value,`"
  (cond
   ((json-edit-is-space c) json-edit-scan-skip-space)
   ((= c ?\")
    (setq json-edit-step 'json-edit-state-in-string)
    json-edit-scan-begin-literal)
   (t (json-edit-set-error c "looking for beginning of object key string"))))


(defun json-edit-state-end-value (c)
  "state after completing a value, such as after reading '{}' or 'true'"
  (catch 'return
    (let ((ps (first json-edit-parse-state)))
      (cond
       ((= 0 (length json-edit-parse-state))
        ;; completed top-level before the current char
        (setq json-edit-step 'json-edit-state-end-top)
        (throw 'return (json-edit-state-end-top c)))
       ((json-edit-is-space c)
        (setq json-edit-step 'json-edit-state-end-value)
        (throw 'return json-edit-scan-skip-space))
       ((eq ps 'json-edit-parse-object-key)
        (when (= c ?:)
          (pop json-edit-parse-state)
          (push 'json-edit-parse-object-value json-edit-parse-state)
          (setq json-edit-step 'json-edit-state-begin-value)
          (throw 'return json-edit-scan-object-key))
        (throw 'return (json-edit-set-error c "after object key")))
       ((eq ps 'json-edit-parse-object-value)
        (when (= c ?,)
          (pop json-edit-parse-state)
          (push 'json-edit-parse-object-key json-edit-parse-state)
          (setq json-edit-step 'json-edit-state-begin-string)
          (throw 'return json-edit-scan-object-value))
        (when (= c ?})
          (pop json-edit-parse-state)
          (throw 'return json-edit-scan-end-object))
        (throw 'return (json-edit-set-error c "after object key:value pair")))
       ((eq ps 'json-edit-parse-array-value)
        (when (= c ?,)
          (setq json-edit-step 'json-edit-state-begin-value)
          (throw 'return json-edit-scan-array-value))
        (when (= c ?\])
          (pop json-edit-parse-state)
          (throw 'return json-edit-scan-end-array))
        (throw 'return (json-edit-set-error c "after array element")))
       (t (throw 'return (json-edit-set-error c "")))))))

(defun json-edit-state-end-top (c)
  "state after finishing the top-level value such as `{}`.
   Only space characters should be seen now"
  (if (json-edit-is-space c)
      json-edit-scan-end
    (json-edit-set-error c "after top-level value")))

(defun json-edit-state-in-string (c)
  (cond
   ((= c ?\")
    (setq json-edit-step 'json-edit-state-end-value)
    json-edit-scan-continue)
   ((= c ?\\)
    (setq json-edit-step 'json-edit-state-in-string-esc)
    json-edit-scan-continue)
   ((< c #x20)
    (json-edit-set-error c "in string literal"))
   (t json-edit-scan-continue)))

(defun json-edit-state-in-string-esc (c)
  (cond
   ((or (= c ?b)
        (= c ?f)
        (= c ?n)
        (= c ?r)
        (= c ?t)
        (= c ?\\)
        (= c ?\"))
    (setq json-edit-step 'json-edit-state-in-string)
    json-edit-scan-continue)
   ((= c ?u)
    (setq json-edit-step 'json-edit-state-in-string-esc-u)
    json-edit-scan-continue)
   (t
    (json-edit-set-error c "in string escape code"))))

(defun json-edit-state-in-string-esc-u (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-edit-step 'json-edit-state-in-string-esc-u1)
    json-edit-scan-continue)
   (t
    (json-edit-set-error c "in \\u hexadecimal character escape"))))

(defun json-edit-state-in-string-esc-u1 (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-edit-step 'json-edit-state-in-string-esc-u12)
    json-edit-scan-continue)
   (t
    (json-edit-set-error c "in \\u hexadecimal character escape"))))

(defun json-edit-state-in-string-esc-u12 (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-edit-step 'json-edit-state-in-string-esc-u123)
    json-edit-scan-continue)
   (t
    (json-edit-set-error c "in \\u hexadecimal character escape"))))

(defun json-edit-state-in-string-esc-u123 (c)
  (cond
   ((or
     (and (<= ?0 c) (<= c ?9))
     (and (<= ?a c) (<= c ?f))
     (and (<= ?A c) (<= c ?F)))
    (setq json-edit-step 'json-edit-state-in-string)
    json-edit-scan-continue)
   (t
    (json-edit-set-error c "in \\u hexadecimal character escape"))))

(defun json-edit-state-neg (c)
  "after reading - during a number"
  (cond
   ((= c ?0)
    (setq json-edit-step 'json-edit-state-0)
    json-edit-scan-continue)
   ((and (<= ?1 c) (<= c ?9))
    (setq json-edit-step 'json-edit-state-1)
    json-edit-scan-continue)
   (t
    (json-edit-set-error c "in numeric literal"))))

(defun json-edit-state-0 (c)
  "after reading `0' during a number"
  (cond
   ((= c ?.)
    (setq json-edit-step 'json-edit-state-dot)
    json-edit-scan-continue)
   ((or (= c ?e) (= c ?E))
    (setq json-edit-step 'json-edit-state-E)
    json-edit-scan-continue)
   (t (json-edit-state-end-value c))))

(defun json-edit-state-1 (c)
  "state-1 is the state after reading a non-zero interger during a number,
such as after reading `1` or `100` but not `0`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-edit-step 'json-edit-state-1)
    json-edit-scan-continue)
   (t (json-edit-state-0 c))))

(defun json-edit-state-dot (c)
  "state afeter reading the integer and decimal point in a number
such as after reading `1.`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-edit-step 'json-edit-state-dot-0)
    json-edit-scan-continue)
   (t (json-edit-set-error c "after decimal point in numeric literal"))))

(defun json-edit-state-dot-0 (c)
  "stae after reading an integer, decimal point, and subsequent digits
of a number, such as after reading `3.14`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-edit-step 'json-edit-state-dot-0)
    json-edit-scan-continue)
   ((or (= c ?e) (= c ?E))
    (setq json-edit-step 'json-edit-state-E)
    json-edit-scan-continue)
   (t (json-edit-state-end-value c))))

(defun json-edit-state-E (c)
  "state after reading the mantissa and e in a number,
such as after reading `314e` or `0.314e`"
  (cond
   ((= c ?+)
    (setq json-edit-step 'json-edit-state-E-sign)
    json-edit-scan-continue)
   ((= c ?-)
    (setq json-edit-step 'json-edit-state-E-sign)
    json-edit-scan-continue)
   (t (json-edit-state-E-sign c))))

(defun json-edit-state-E-sign (c)
  "state after reading the mantissa, e, and sign in a number,
such as after reading `314e-` or `0.314e+`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-edit-step 'json-edit-state-E-0)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in exponent of numeric literal"))))

(defun json-edit-state-E-0 (c)
  "state after reading mantissa, e, optional sign, and at least
one digit of the exponent in a number, such as `314e-2`"
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (setq json-edit-step 'json-edit-state-E-0)
    json-edit-scan-continue)
   (t (json-edit-state-end-value c))))

(defun json-edit-state-t (c)
  "state after reading `t`"
  (cond
   ((= c ?r)
    (setq json-edit-step 'json-edit-state-tr)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal true (expecting 'r')"))))

(defun json-edit-state-tr (c)
  "state after reading `tr`"
  (cond
   ((= c ?u)
    (setq json-edit-step 'json-edit-state-tru)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal true (expecting 'u')"))))

(defun json-edit-state-tru (c)
  "state after reading `tru`"
  (cond
   ((= c ?e)
    (setq json-edit-step 'json-edit-state-end-value)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal true (expecting 'e')"))))

(defun json-edit-state-f (c)
  "state after reading `f`"
  (cond
   ((= c ?a)
    (setq json-edit-step 'json-edit-state-fa)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal false (expecting 'a')"))))

(defun json-edit-state-fa (c)
  "state after reading `fa`"
  (cond
   ((= c ?l)
    (setq json-edit-step 'json-edit-state-fal)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal false (expecting 'l')"))))

(defun json-edit-state-fal (c)
  "state after reading `fal`"
  (cond
   ((= c ?s)
    (setq json-edit-step 'json-edit-state-fals)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal false (expecting 's')"))))

(defun json-edit-state-fals (c)
  "state after reading `fals`"
  (cond
   ((= c ?e)
    (setq json-edit-step 'json-edit-state-end-value)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal false (expecting 'e')"))))

(defun json-edit-state-n (c)
  "state after reading `n`"
  (cond
   ((= c ?u)
    (setq json-edit-step 'json-edit-state-nu)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal null (expecting 'u')"))))

(defun json-edit-state-nu (c)
  "state after reading `nu`"
  (cond
   ((= c ?l)
    (setq json-edit-step 'json-edit-state-nul)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal null (expecting 'l')"))))

(defun json-edit-state-nul (c)
  "state after reading `nu`"
  (cond
   ((= c ?l)
    (setq json-edit-step 'json-edit-state-end-value)
    json-edit-scan-continue)
   (t (json-edit-set-error c "in literal null (expecting 'l')"))))

(provide 'json-edit)

;;; json-edit.el ends here
