(in-package :lapis)

(export '(send-message
          message-payload))

(defstruct message
  (sender nil :type (or string null))
  (receiver nil :type (or string null))
  (type "generic")
  (payload))


(defun add-to-message-list (receiver message delivery-type)
  (declare (game-object receiver)
           (message message)
           (symbol delivery-type))
  (ecase delivery-type
    (:sync (push message (game-object-messages receiver)))
    (:async (funcall (game-object-message-handler-func receiver) message receiver))))


(defun pass-to-broadcast-receivers (name message delivery-type)  
  (declare (string name)
           (message message)
           (symbol delivery-type))
  (let ((receivers (find-receivers name)))
    (loop for rec in receivers do
         (add-to-message-list rec message delivery-type))))

(defun send-message (&key sender receiver type payload (delivery-type :sync))
  (declare ((or game-object string null) sender receiver))
  (let ((mes (make-message :sender (typecase sender
                                     (string sender)
                                     (game-object (game-object-name sender)))
                           :receiver (typecase receiver
                                       (string receiver)
                                       (game-object (game-object-name receiver)))
                           :type type :payload payload)))
    (if receiver
        (add-to-message-list (typecase receiver
                               (string (find-object receiver))
                               (game-object receiver))
                             mes delivery-type)
        (pass-to-broadcast-receivers type mes delivery-type))))
  
  