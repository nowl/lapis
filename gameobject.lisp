(in-package :lapis)

(defstruct game-object
  (name)
  (render-func)
  (update-func)
  (message-handler-func)
  (messages))