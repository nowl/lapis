(in-package :lapis)

(defstruct game-object
  (name)
  (render-func)
  (update-func)
  (message-handler-func)
  (messages))

(defun process-messages (obj)
  (declare (game-object obj))
  (loop for message in (game-object-messages obj) do
       (funcall (game-object-message-handler-func obj) message obj))
  (setf (game-object-messages obj) nil))