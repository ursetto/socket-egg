;;; A short example of a client using unix sockets and the socket egg.

;; Source: http://wiki.call-cc.org/Communicating%20over%20a%20unix%20socket,%20using%20the%20socket%20egg

(require-extension socket)
(cond-expand
  (chicken-5
    (import (chicken format))))

;;; Create a new unix socket:
(let* ((unix-socket (socket af/unix sock/stream))
       (pathname "foo") ; Where to bind the socket to
       (message-to-send "this is a test")) ; What to send to the server
  ;; Bind the file "foo" to the unix socket:
  (socket-connect unix-socket (unix-address pathname))
  (let ((number-of-bytes-sent (socket-send unix-socket message-to-send)))
    (printf "Number of bytes sent to the server: ~A~%" number-of-bytes-sent))
  ;; Cleanup:
  (socket-close unix-socket))
