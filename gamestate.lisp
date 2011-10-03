(in-package :lapis)

(export '(make-gamestate))

(defparameter *gamestate* nil)

(defstruct gamestate
  (name)
  (objects-by-name (make-hash-table :test #'equal))
  (broadcast-receivers (make-hash-table :test #'equal)))

(defun find-object (name-or-obj)
  (declare ((or string game-object) name-or-obj))
  (etypecase name-or-obj
    (string (gethash name-or-obj (gamestate-objects-by-name *gamestate*)))
    (game-object name-or-obj)))

(defun find-receivers (name)
  (declare (string name))
  (gethash name (gamestate-broadcast-receivers *gamestate*)))
  
(defun make-broadcast-receiver (obj type)
  (declare (string type)
           ((or game-object string) obj))
  (let ((o (find-object obj)))
    (multiple-value-bind (obj-list win)
        (gethash type (gamestate-broadcast-receivers *gamestate*))
      (setf (gethash type (gamestate-broadcast-receivers *gamestate*))
            (if win
                (pushnew o obj-list)
                (list o))))))

(defun new-game-object (name &key render-func update-func message-handler-func)
  (let ((go (make-game-object :name name 
                              :render-func render-func :update-func update-func 
                              :message-handler-func message-handler-func)))
    (setf (gethash name (gamestate-objects-by-name *gamestate*))
          go)))