;;; A short example of a server using unix sockets and the socket egg.

;; Source: http://wiki.call-cc.org/Communicating%20over%20a%20unix%20socket,%20using%20the%20socket%20egg

(require-extension socket)
(cond-expand
  (chicken-5
    (import (chicken format) (chicken file))))

;;; Create a new unix socket:
(let ((unix-socket (socket af/unix sock/stream))
      (backlog 1) ; Length of connection queue for socket-listen call below
      (socket-pathname "foo")) ; Where to bind the socket to
  ;; Bind the file to the socket:
  (socket-bind unix-socket (unix-address socket-pathname))
  ;; Listen for incoming connections:
  (socket-listen unix-socket backlog)
  ;; Read and display a message from the client.
  (let* ((connected-socket (socket-accept unix-socket))
         (message-length 14) ; We must know the length in advance, or negotiate it.
         (received-data (socket-receive connected-socket message-length)))
    ;; Report what was read:
    (printf "Data received from the client: '~A'~%" received-data)
    ;; Cleanup:
    (socket-close connected-socket)
    (socket-close unix-socket)
    (delete-file socket-pathname)  ;; FIXME: do this as cleanup
    ))
