;breaks line at an intersection 

(defun c:bi ( / ss point snap echo olderr)
  (setq snap (getvar "osmode"))
  (setq echo (getvar "cmdecho"))
  (setq olderr *error*)
  (setq *error* break_error)
  (setvar "cmdecho" 0)
  (while (not ss)
    (setq ss (entsel "\nSelect line to break: "))
  );while
  (setvar "osmode" 32)
  (while (not point)
    (setq point
    (osnap (getpoint "\nSelect intersection to break at: ") "int"))
  );while
  (setvar "osmode" snap)
  (command "break" ss "f" point "@")
  (setvar "cmdecho" echo)
  (setq *error* olderr)
  (princ)
)

(defun break_error (msg)
  (princ (strcat "\nError: " msg))
  (setvar "osmode" 0)
  (setvar "cmdecho" 1)
  (princ)
)
