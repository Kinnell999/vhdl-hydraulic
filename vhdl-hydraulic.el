;------------------------------------------------------------------------------
; This program is free software: you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation, either version 3 of the License, or
;     (at your option) any later version.
; 
;     This program is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.
; 
;     You should have received a copy of the GNU General Public License
;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;------------------------------------------------------------------------------
; Copyright (c) David Fraser 2014
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Settings
;------------------------------------------------------------------------------

(setq vh-sm-synchronous? t)

(setq vh-default-header 
"----------------------------------------------------------------------------
-- %label%
----------------------------------------------------------------------------
-- %comment%
----------------------------------------------------------------------------")

(setq vh-process-header 
"----------------------------------------------------------------------------
-- Process: %label%
----------------------------------------------------------------------------
-- %comment%
----------------------------------------------------------------------------")

(setq vh-procedure-header
"----------------------------------------------------------------------------
-- Procedure: %label%
----------------------------------------------------------------------------
-- %comment%
----------------------------------------------------------------------------")

(setq vh-function-header 
"----------------------------------------------------------------------------
-- Function: %label%
----------------------------------------------------------------------------
-- %comment%
----------------------------------------------------------------------------")

;------------------------------------------------------------------------------
; Main interactive functions
;------------------------------------------------------------------------------

(defun vh-procedure (label &optional comment)
"Inserts a procedure template in the current buffer at point"
  (interactive "sLabel: ")
  (save-excursion
    (vh-insert 0
	       (vh-header (if vh-procedure-header vh-procedure-header vh-default-header)
			  (list (cons "%label%" label)
				(cons "%comment%" (if comment comment ""))))
	       (concat "PROCEDURE " label "() IS")
	       "BEGIN"
	       "END PROCEDURE;"))
  (search-forward (concat "PROCEDURE " label "(")))

(defun vh-function (label &optional comment)
"Inserts a function template in the current buffer at point"
  (interactive "sLabel: ")
  (save-excursion
    (vh-insert 0
	       (vh-header (if vh-function-header vh-function-header vh-default-header)
			  (list (cons "%label%" label)
				(cons "%comment%" (if comment comment ""))))
	       (concat "FUNCTION " label "() IS")
	       "BEGIN"
	       "END FUNCTION;"))
  (search-forward (concat "FUNCTION " label "(")))

(defun vh-process-async (label &optional async-clause s-list comment)
"Inserts an asynchronous process template in the current buffer at point"
  (interactive "sLabel: ")
  (beginning-of-line)
  (vh-insert 1
	     (vh-header vh-process-header
			(list (cons "%label%" label)
			      (cons "%comment%" (if comment comment ""))))
	     (concat label " : PROCESS(" (if s-list s-list "") ")")
	     "BEGIN"
	     (vh-indent-n 1 async-clause)
	     (concat "END PROCESS " label ";")
	     ""))

(defun vh-process-sync (label &optional reset-clause clock-clause comment)
"Inserts a synchronous process template in the current buffer at point"
  (interactive "sLabel: ")
  (let ((clock-cond (vh-clk-clause))
	(reset-cond (concat vhdl-reset-name " = " 
			    (if vhdl-reset-active-high vhdl-one-string 
			      vhdl-zero-string)))
	(sensitivity-list (concat (vh-clk-name)
				  (if (eq vhdl-reset-kind 'async) ", rst_n" ""))))
    (vh-process-async label
		      (cond  
		       ((eq vhdl-reset-kind 'async) 
			(vh-if-template
			 (cons reset-cond reset-clause)
			 (cons clock-cond clock-clause)))
		       ((eq vhdl-reset-kind 'sync)
			(vh-if-template 
			 (cons clock-cond 
			       (vh-if-template (cons reset-cond reset-clause)
					       clock-clause))))
		       ((eq vhdl-reset-kind 'none) 
			(vh-if-template (cons clock-cond clock-clause)))
		       sensitivity-list comment))))

(defun vh-process-del (label)
  "Deletes a process named label and all comment lines immediately preceding it"
  (interactive "sLabel: ")
  (save-excursion 
    (goto-char (point-min))
    (vh-kill-line-re (concat label " +: +PROCESS")
		     (concat "END +PROCESS +" label))
    (let ((end (point)))
      (forward-line -1)
      (while (re-search-forward "\\W+--" (line-end-position) t)
	(forward-line -1))
      (forward-line)
      (kill-region (line-beginning-position) end))))

(defun vh-sm (str)
"Prompts the user for a list of states then inserts a state machine template"
  (interactive "sStates: ")
  (let ((states (split-string str)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^\\W*ARCHITECTURE\\W.+\\WOF\\W.+\\WIS\\W*$")
	  (progn 
	    (forward-char)
	    (vh-insert 1 ""
		       (concat "TYPE state_type IS (" (mapconcat 'identity states ", ") ");")
		       ""
		       "SIGNAL state      : state_type;"
		       "SIGNAL next_state : state_type;"
		       "")))
      (goto-char (point-max))
      (if (re-search-backward "^\\W*END\\W+ARCHITECTURE")
	  (progn 
	    (vh-process-sync "state_register" 
			     (concat "state <= " (car states) ";")
			     "state <= next_state;")
	    (if vh-sm-synchronous?
		(vh-process-sync "state_output" ""
				 (vh-case-template "next_state" 
						   (append states '("OTHERS"))))
	      (vh-process-async "state_output"
				(vh-case-template "state" 
						  (append states '("OTHERS")))
				"state"))
	    (vh-process-async "state_transition" 
			      (vh-case-template "state" (append states '("OTHERS")))
			      "state"))))))
  
(defun vh-sm-add (str)
"Prompts the user for a list of states then adds them to an exisitng state
 machine template"
  (interactive "sAdd States: ")
  (let ((states (split-string str)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "TYPE\\W+state_type\\W+IS\\W+\(" nil t)
	  (progn 
	    (re-search-forward ".*\)")
	    (backward-char)
	    (insert (concat  ", " (mapconcat 'identity states ", ")))
	    (search-forward "state_output : PROCESS")
	    (search-forward "END PROCESS")
	    (search-backward "WHEN OTHERS =>")
	    (beginning-of-line)
	    (vh-insert (if 'vh-sm-synchronous? 
			   (if (eq vhdl-reset-kind 'sync) 5 4) 3) (vh-when-template states))
	    (search-forward "state_transition : PROCESS")
	    (search-forward "END PROCESS")
	    (search-backward "WHEN OTHERS =>")
	    (beginning-of-line)
	    (vh-insert 3 (vh-when-template states)))))))

(defun vh-sm-del (str)
 "Prompts the user for state name then removes them from an existing state
  machine template"
  (interactive "sDelete State: ")
  (if (> (length str) 0)
      (let ((limit)
	    (re-term (concat ", *" str " *\\| *" str " *, *\\| *" str " *")))
	(save-excursion
	  (goto-char (point-min))
	  (if (search-forward "TYPE state_type IS ")
	      (progn 
		(search-forward ")")
		(setq limit (point))
		(search-backward "(")
		(if (re-search-forward re-term limit t) 
		    (replace-match ""))
		(save-excursion 
		  (while (search-forward "WHEN")
		    (let ((end (- (point) 4)))
		      (if (re-search-backward (concat "WHEN\\W+" str "\\W+=>") (point-min) t)
			  (kill-region (point) end)))))
		(save-excursion 
		  (perform-replace (concat "<=\\W+" str) "<= " nil t nil))))))
    (if (y-or-n-p "Delete entire state machine? ")
	(save-excursion
	  (goto-line (point-min))
	  (vh-kill-line-re "TYPE\\W+state_type\\W+IS\\W+\(" "\)")
	  (vh-kill-line-re "SIGNAL\\W+state\\W+:\\W+state_type\\W*;")
	  (vh-kill-line-re "SIGNAL\\W+next_state\\W+:\\W+state_type\\W*;")
	  (vh-process-del "state_register")
	  (vh-process-del "state_output")
	  (vh-process-del "state_transition")))))


;------------------------------------------------------------------------------
; Helper Functions
;------------------------------------------------------------------------------

(defun vh-header (header &optional subs)
  "Returns a processed header string with text substitutions made based on subs.
Parameter subs is a list of cons cells with the car containing the text to be substituted and the cdr containing the replacement text."
  (if subs
      (vh-header (replace-regexp-in-string (caar subs) (cdar subs) header) (cdr subs))
    header))

(defun vh-indent-n (level &rest text-list)
  "Constructs a string from a list of strings, and indents each line in the resultant string
by level*vhdl-basic-offset.  Each string argument is considered a seperate line."
  (let ((indent (mapconcat 'identity (make-list level vhdl-basic-offset) ""))
	(text (mapconcat 'identity (delq nil text-list) "\n")))
     (mapconcat (lambda (string) 
		  (if (= (length string) 0)
		      string
		      (concat indent string)))
		(split-string text "\n")
		"\n")))

(defmacro vh-insert (level &rest text-list)
  "Inserts a list of lines, and indents each by level*vhdl-basic-offset+current indentation"
  (let ((current-indent (make-string  (- (point) (line-beginning-position)) ?\s)))
    `(progn (beginning-of-line)
	    (insert 
	     (mapconcat (lambda (line) (concat ,current-indent line))
			(split-string (vh-indent-n ,level ,@text-list) "\n")
			"\n"))
	    (insert "\n"))))

(defun vh-kill-line-re (first &optional last)
  "Kills the line where re first is matched.  If last is specified, all lines between where
 first and last match inclusive are killed"
  (let ((start (if (re-search-forward first (point-max) )
		   (line-beginning-position)))
	(end (if last
		 (if (re-search-forward last (point-max) )
		     (line-end-position)))))
    (if start
	(if end (kill-region start (+ end 1))
	  (kill-region start (+ (line-end-position) 1))))))

(defun vh-find-last (regex)
  "Searches for the last occurrence of regex in the buffer"
  (save-excursion
    (progn
      (goto-char (point-max))
      (re-search-backward regex nil t))))
  
(defun vh-clk-clause nil
  "Searches the buffer for a clock condition and returns it if found, otherwise the default
 condition"
  (if (vh-find-last " \\(.+_edge\(\w+\)\\)")
      (match-string-no-properties 1)
    (concat "rising_edge(" vhdl-clock-name ")")))

(defun vh-clk-name nil
  "Searches the buffer for a clock name and returns it if found, otherwise the default
 name"
  (if (vh-find-last " .+_edge\(\\(\w+\\)\)")
      (match-string-no-properties 1)
    vhdl-clock-name))

(defun vh-case-template (variable &optional values)
  "Constructs a VHDL case statement.  If list values is specified, a when statement is also 
generated for each string in values." 
  (vh-indent-n 0 
	     (concat "CASE " variable " IS")
	     (if values 
		 (vh-indent-n 1 (mapconcat (lambda (v) (concat "WHEN " v " =>")) 
					   values "\n")))
	     "END CASE;"))

(defun vh-else-template (clause-list)
  "Generates a sequence of ELSIF/ELSE statements. Helper for vh-if-template"
  (if clause-list
      (let ((clause (car clause-list)))
	(if (stringp clause)
	    (vh-indent-n 0 "ELSE" 
			 (vh-indent-n 1 clause)
			 "END IF;")
	  (vh-indent-n 0 (concat "ELSIF " (car clause) " THEN")
		       (vh-indent-n 1 (cdr clause))
		       (vh-else-template (cdr clause-list)))))
    "END IF;"))

(defun vh-if-template (if-clause &rest clause-list)
  "Generates IF...ELSIF...ELSE statements. if-clause must be a cons cell of the condition 
and statement. clause-list contains either more of these to generate ELSIFs, or a string
to generate an ELSE statement"  
    (vh-indent-n 0 (concat "IF " (car if-clause) " THEN")
		 (vh-indent-n 1 (cdr if-clause))
		 (vh-else-template clause-list)))
