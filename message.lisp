(in-package :lapis)

(export '(send-message))

(defstruct message
  (sender nil :type (or game-object null))
  (receiver nil :type (or game-object null))
  (type "generic" :type string)
  (payload))


(defun add-to-message-list (receiver message delivery-type)
  (declare (game-object receiver)
           (message message)
           (symbol delivery-type))
  (ecase delivery-type
    (:sync (push message (game-object-messages receiver)))
    (:async (funcall (game-object-message-handler-func receiver) message))))


(defun pass-to-broadcast-receivers (name message delivery-type)
  (declare (string name)
           (message message)
           (symbol delivery-type))
  (let ((receivers (find-receivers name)))
    (loop for rec in receivers do
         (add-to-message-list rec message delivery-type))))

(defun send-message (&key sender receiver type payload (delivery-type :sync))
  (declare ((or game-object string null) sender receiver)
           (string type))  
  (let ((mes (make-message :sender (typecase sender
                                     (string (find-object sender))
                                     (t sender))
                           :receiver (typecase receiver
                                       (string (find-object receiver))
                                       (t receiver))
                           :type type :payload payload)))
    (if receiver
        (add-to-message-list receiver mes delivery-type)
        (pass-to-broadcast-receivers type mes delivery-type))))
  
  