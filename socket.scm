;; UNIX sockets not supported because they do not exist on Windows (though we could test for this)

;; socket-accept should perhaps return connected peer address
;; not all errors close the socket (probably should) -- e.g., recv failure, send failure;
;;     however, on a timeout, ports require that the socket stay open
;; implement socket-receive
;; output line buffering not implemented
;; socket ports work with datagrams

;; Socket type (slot 7) is deliberately set to "socket6" instead of "socket" to prevent
;; port->fileno from accessing port data (which is in a different format).  This is hardcoded
;; in the core library.

(use foreigners)
(use srfi-4)

(foreign-declare "
#include <errno.h>
#ifdef _WIN32
/* Unconditionally require Windows XP (0x501) for getaddrinfo.  It is not known
   how to autodetect missing getaddrinfo support.  These funcs are supported
   on W2k and even earlier with native SDK (Wspiapi.h), but MinGW does not support them. */
#define _WIN32_WINNT 0x501

# if (defined(HAVE_WINSOCK2_H) && defined(HAVE_WS2TCPIP_H))
#  include <winsock2.h>
#  include <ws2tcpip.h>
# else
#  include <winsock.h>
# endif
/* Beware: winsock2.h must come BEFORE windows.h */
# define socklen_t       int
static WSADATA wsa;
# define fcntl(a, b, c)  0

#ifndef SHUT_RD
# define SHUT_RD SD_RECEIVE
#endif
#ifndef SHUT_WR
# define SHUT_WR SD_SEND
#endif
#ifndef SHUT_RDWR
# define SHUT_RDWR SD_BOTH
#endif

# define typecorrect_getsockopt(socket, level, optname, optval, optlen)	\\
    getsockopt(socket, level, optname, (char *)optval, optlen)

/* On Windows < Vista, getnameinfo is broken, erroring out if it cannot resolve the
   service to a name.  Try to detect this case and rerun the call with NUMERICSERV set
   to avoid the error.  Note: we should check Windows version but do not. */
int WSAAPI skt_getnameinfo(const struct sockaddr *sa, socklen_t salen, char *node,
  DWORD nodelen, char *service, DWORD servicelen, int flags) {
    int rc = getnameinfo(sa,salen,node,nodelen,service,servicelen,flags);
    int err = WSAGetLastError();      /* rc *might* not be gai error, though it should be */
    if (rc != 0 && !(flags & NI_NUMERICSERV) && err == WSANO_DATA) {
        rc = getnameinfo(sa,salen,node,nodelen,service,servicelen,flags | NI_NUMERICSERV);
        err = WSAGetLastError();
    }
    return rc ? err : 0;
}

#define ECONNREFUSED WSAECONNREFUSED
#define ETIMEDOUT WSAETIMEDOUT
/* May need to test WSAEHOSTDOWN as well */
#define ENETUNREACH WSAENETUNREACH
#define EHOSTUNREACH WSAEHOSTUNREACH
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EINPROGRESS WSAEINPROGRESS
#define ENOTCONN WSAENOTCONN
/* Note that it may be possible to replace all references to errno/GetLastError() with
   a getsockopt(SO_ERROR). */
#define errno (WSAGetLastError())

char *skt_strerror(int err) {
    static char msg[1024];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS |
                  FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, err,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (char*)msg, 1024, NULL);
    return msg;
}

#else
# include <fcntl.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <netinet/in.h>
# include <unistd.h>
# include <netdb.h>
# include <signal.h>
# define closesocket     close
# define INVALID_SOCKET  -1
# define typecorrect_getsockopt getsockopt
# define skt_getnameinfo getnameinfo
# define skt_strerror strerror
#endif

#ifdef ECOS
#include <sys/sockio.h>
#endif

")

;;; constants

(define-foreign-enum-type (address-family int)
  (address-family->integer integer->address-family)
  ((af/unspec AF_UNSPEC) AF_UNSPEC)
  ((af/inet AF_INET) AF_INET)
  ((af/inet6 AF_INET6) AF_INET6)
  ((af/unix AF_UNIX) AF_UNIX))
(define af/inet AF_INET)
(define af/inet6 AF_INET6)
(define af/unix AF_UNIX)

(define-foreign-enum-type (socket-type int)
  (socket-type->integer integer->socket-type)
  ((sock/stream SOCK_STREAM) SOCK_STREAM)
  ((sock/dgram  SOCK_DGRAM)  SOCK_DGRAM)
  ((sock/raw    SOCK_RAW)    SOCK_RAW))
(define sock/stream SOCK_STREAM)
(define sock/dgram  SOCK_DGRAM)
(define sock/raw    SOCK_RAW)

(define-foreign-enum-type (protocol-type int)
  (protocol-type->integer integer->protocol-type)
  ((ipproto/tcp IPPROTO_TCP)  IPPROTO_TCP)
  ((ipproto/udp IPPROTO_UDP)  IPPROTO_UDP))
(define ipproto/tcp IPPROTO_TCP)
(define ipproto/udp IPPROTO_UDP)

(define-foreign-variable AI_CANONNAME int "AI_CANONNAME")
(define-foreign-variable AI_NUMERICHOST int "AI_NUMERICHOST")
(define-foreign-variable AI_PASSIVE int "AI_PASSIVE")
(define ai/canonname AI_CANONNAME)
(define ai/numerichost AI_NUMERICHOST)
(define ai/passive AI_PASSIVE)
(define-foreign-variable NI_MAXHOST int "NI_MAXHOST")
(define-foreign-variable NI_MAXSERV int "NI_MAXSERV")

(define-foreign-variable NI_NUMERICHOST int "NI_NUMERICHOST")
(define-foreign-variable NI_NUMERICSERV int "NI_NUMERICSERV")
(define-foreign-variable NI_DGRAM int "NI_DGRAM")
(define-foreign-variable NI_NAMEREQD int "NI_NAMEREQD")
(define-foreign-variable NI_NOFQDN int "NI_NOFQDN")
(define ni/numerichost NI_NUMERICHOST)
(define ni/numericserv NI_NUMERICSERV)
(define ni/dgram NI_DGRAM)
(define ni/namereqd NI_NAMEREQD)
(define ni/nofqdn NI_NOFQDN)

;;;

(define-foreign-record-type (sa "struct sockaddr")
  (int sa_family sa-family))

(define-record sockaddr family blob)

(define (sa->sockaddr sa len)    ;; sa -- c-pointer; len -- length of sockaddr struct
  (make-sockaddr (sa-family sa)
                 (let ((b (make-blob len)))
                   ((foreign-lambda void C_memcpy scheme-pointer c-pointer int)
                    b sa len)
                   b)))

(define (sockaddr-len A)
  (blob-size (sockaddr-blob A)))
;; (define (sockaddr-path A)          ;; not supported on Windows
;;   ((foreign-lambda* c-string ((scheme-pointer sa))
;;      "switch (((struct sockaddr*)sa)->sa_family) {"
;;      "case AF_UNIX: C_return(((struct sockaddr_un*)sa)->sun_path);"
;;      "default: C_return(NULL); }"
;;      )
;;    (sockaddr-blob A)))
(define (sockaddr-path A)
  (error 'sockaddr-path "UNIX sockets are not supported"))

(define (sockaddr-address A)
  (let ((af (sockaddr-family A)))
    (cond ((or (= af AF_INET)
               (= af AF_INET6))
           (car (getnameinfo A (+ NI_NUMERICHOST NI_NUMERICSERV))))
          ((= af AF_UNIX)
           (sockaddr-path A))
          (else #f))))
(define (sockaddr-port A)
  ((foreign-lambda* scheme-object ((scheme-pointer sa))
     "switch (((struct sockaddr*)sa)->sa_family) {"
     "case AF_INET: C_return(C_fix(ntohs(((struct sockaddr_in*)sa)->sin_port)));"
     "case AF_INET6: C_return(C_fix(ntohs(((struct sockaddr_in6*)sa)->sin6_port)));"
     "default: C_return(C_SCHEME_FALSE); }")
   (sockaddr-blob A)))

(define-record-printer (sockaddr A out)
  (fprintf out "#<sockaddr ~S>"
           (sockaddr->string A)
           ;; (integer->address-family (sockaddr-family A))
           ))

;; Convert socket address/path to a compact string, mainly for display purposes.
(define (sockaddr->string A)
  (let ((af (sockaddr-family A)))
    (cond ((or (= af AF_INET)
               (= af AF_INET6))
           (let* ((ni (getnameinfo A (+ NI_NUMERICHOST NI_NUMERICSERV)))
                  (h (car ni))
                  (p (cdr ni)))
             (if (string=? p "0")
                 h
                 (if (= af AF_INET6)
                     (string-append "[" h "]" ":" p)
                     (string-append h ":" p)))))
          ((= af AF_UNIX)
           (sockaddr-path A))  ;; or reach directly into blob here
          (else
           #f))))

;; Intent of this is a direct call to getnameinfo ala inet_ntop, returning
;; a plain string; however, error handling is hard.
;; (define (sockaddr->ip-string A)
;;   (foreign-lambda* c-string ((scheme-pointer sa))
;;     ""
;;     ))

(define-foreign-record-type (ai "struct addrinfo")
  (constructor: alloc-ai)
  (destructor: free-ai)   ; similar name!
  (int ai_flags ai-flags set-ai-flags!)
  (int ai_family ai-family set-ai-family!)
  (int ai_socktype ai-socktype set-ai-socktype!)
  (int ai_protocol ai-protocol set-ai-protocol!)  
  (int ai_addrlen ai-addrlen)
  ((c-pointer sa) ai_addr ai-addr)  ;; non-null?
  (c-string ai_canonname ai-canonname)
  ((c-pointer ai) ai_next ai-next))

(define-syntax non-nil
  (syntax-rules ()
    ((_ a)
     (let ((x a))
       (if (or (not x) (null? x)) #f x)))
    ((_ a . rest)
     (let ((x a))
       (if (or (not x) (null? x))
           (non-nil . rest)
           x)))))

(define-record addrinfo
  flags family socktype protocol address canonname)
(define-record-printer (addrinfo a out)
  (fprintf out "#<addrinfo ~S ~S ~S ~S~A>"
           (sockaddr->string (addrinfo-address a))
           (non-nil (integer->address-family (addrinfo-family a)) (addrinfo-family a))
           (non-nil (integer->socket-type (addrinfo-socktype a)) (addrinfo-socktype a))
           (non-nil (integer->protocol-type (addrinfo-protocol a)) (addrinfo-protocol a))
           (cond ((addrinfo-canonname a)
                  => (lambda (cn) (sprintf " canonical: ~S" cn)))
                 (else ""))
           ;; (addrinfo-flags a)          ;; flag display isn't that interesting
           ))

(define (ai->addrinfo ai)           ;; construct addrinfo obj from ai ptr, with embedded sockaddr obj
  (make-addrinfo
   (ai-flags ai)
   (ai-family ai)
   (ai-socktype ai)
   (ai-protocol ai)
   (ai->sockaddr ai)
   (ai-canonname ai)))
(define (ai->sockaddr ai)           ;; direct construction of sockaddr object from ai pointer
  (and-let* ((addr (ai-addr ai)))
    (sa->sockaddr addr (ai-addrlen ai))))

(define (ai-list->addrinfo ai)      ;; construct addrinfo object list from ai linked list
  (let loop ((ai ai)
             (L '()))
    (if ai
        (loop (ai-next ai)
              (cons (ai->addrinfo ai) L))
        (reverse L))))

(define (alloc-null-ai)
  (let ((null! (foreign-lambda* void ((ai ai))
                 "memset(ai,0,sizeof(*ai));"
                 ))
        (ai (alloc-ai)))
    (null! ai)
    ai))
(define _getaddrinfo
    (foreign-lambda int getaddrinfo c-string c-string ai (c-pointer ai)))
(define freeaddrinfo
  (foreign-lambda void freeaddrinfo ai))
(define _getnameinfo
  (foreign-lambda int skt_getnameinfo scheme-pointer int scheme-pointer
                  int scheme-pointer int int))

(define gai_strerror (foreign-lambda c-string "gai_strerror" int))

(define-foreign-variable eai/noname int "EAI_NONAME")

;; FIXME: hints constructor is craaaap
;; Returns a c-pointer; must call freeaddrinfo on result once used.
(define (getaddrinfo/ai node service family socktype protocol flags)
  (let-location ((res c-pointer))
    (let ((hints #f))
      (define hints (alloc-null-ai))
      (when family (set-ai-family! hints family))
      (when socktype (set-ai-socktype! hints socktype))
      (when flags (set-ai-flags! hints flags))
      (when protocol (set-ai-protocol! hints protocol))
      (let ((rc (_getaddrinfo node service hints #$res)))
        (when hints (free-ai hints))
        (cond ((= 0 rc)
               res)
              ((= eai/noname rc)  ;; save exceptions for real errors
               #f)
              (else
               (when res (freeaddrinfo res))   ;; correct??
               (error 'getaddrinfo (gai_strerror rc) node)))))))
(define (getaddrinfo node service family socktype protocol flags)
  (let* ((ai (getaddrinfo/ai node service family socktype protocol flags))
         (addrinfo (ai-list->addrinfo ai)))
    (when ai (freeaddrinfo ai))
    addrinfo))

(define (address-information node service #!key family type protocol flags)
  (let ((service (if (integer? service) (number->string service) service)))
    (getaddrinfo node service family type protocol flags)))

;; Constructor for socket address object from IP address string & SERVICE number.
;; The usual way to create such an address is via address-information; this is
;; a more efficient shortcut.
;; FIXME: The name is suspect.
(define (inet-address ip #!optional service)   ;; Not sure if optional service makes sense.
  (let ((service (and service (number->string service))))
    (and-let* ((ai (getaddrinfo/ai ip service #f #f #f AI_NUMERICHOST))  ;; + AI_NUMERICSERV
               (saddr (ai->sockaddr ai)))
      (freeaddrinfo ai)
      saddr)))

;; ADDR is either a SOCKADDR object, or an IPv4 or IPv6 string.
;; Converts returned port to numeric if possible.  Does not convert 0 to #f though.
;; Note: Should add AI_NUMERICSERV to getaddrinfo call, but it may not be portable.
;; Note: Perhaps service should be mandatory.
;; Note: (car (name-information addr flags: ni/numerichost)) ==
;;         (sockaddr-address (inet-sockaddr addr)), so there is some redundancy.
(define (name-information addr service #!key (flags 0))
  (define (massage ni)
    (cond ((string->number (cdr ni))
           => (lambda (p) (cons (car ni) p)))
          (else ni)))
  (cond
   ((sockaddr? addr)
    (massage (getnameinfo addr flags)))         ; service ignored
   (else
    (let ((port (cond ((not service) #f)
                      ((integer? service) service)
                      ((string->number service))
                      (else (error 'name-information "service must be a numeric value or #f"
                                   service)))))
      (let ((saddr (inet-address addr port)))
        (unless saddr
          (error 'name-information "invalid internet address" addr port))
        (massage (getnameinfo saddr flags)))))))

(define (getnameinfo saddr flags)
  (let* ((sa (sockaddr-blob saddr))
         (salen (sockaddr-len saddr)))
    (let ((node (make-string NI_MAXHOST))
          (serv (make-string NI_MAXSERV)))
      (let ((rc (_getnameinfo sa salen
                              node NI_MAXHOST
                              serv NI_MAXSERV flags)))
        (cond ((= rc 0)
               (cons (substring node 0 (string-index node #\nul))
                     (substring serv 0 (string-index serv #\nul))))
              (else
               (error 'getnameinfo (gai_strerror rc))))))))



;;; socket operations

(define socket-startup
  (foreign-lambda* bool () "
#ifdef _WIN32
     C_return(WSAStartup(MAKEWORD(1, 1), &wsa) == 0);
#else
     signal(SIGPIPE, SIG_IGN);
     C_return(1);
#endif
"))

(unless (socket-startup)   ;; hopefully, this is safe to run multiple times
  (network-error 'socket-startup "cannot initialize socket code"))

(define socket-connect-timeout)
(define socket-read-timeout)
(define socket-write-timeout)
(define socket-accept-timeout)

(let ()
  (define ((check loc) x)
    (when x (##sys#check-exact x loc))
    x)
  (define minute (fx* 60 1000))
  (set! socket-read-timeout (make-parameter minute (check 'socket-read-timeout)))
  (set! socket-write-timeout (make-parameter minute (check 'socket-write-timeout))) 
  (set! socket-connect-timeout (make-parameter #f (check 'socket-connect-timeout))) 
  (set! socket-accept-timeout (make-parameter #f (check 'socket-accept-timeout))) )

(define-foreign-variable errno int "errno")
(define-foreign-variable strerrno c-string "skt_strerror(errno)")
(define-foreign-variable _invalid_socket int "INVALID_SOCKET")
(define-foreign-variable _ewouldblock int "EWOULDBLOCK")
(define-foreign-variable _einprogress int "EINPROGRESS")
(define-foreign-variable _econnrefused int "ECONNREFUSED")
(define-foreign-variable _etimedout int "ETIMEDOUT")
(define-foreign-variable _enetunreach int "ENETUNREACH")
(define-foreign-variable _ehostunreach int "EHOSTUNREACH")
(define-foreign-variable _enotconn int "ENOTCONN")

(define-foreign-variable SHUT_RD int "SHUT_RD")
(define-foreign-variable SHUT_WR int "SHUT_WR")
(define-foreign-variable SHUT_RDWR int "SHUT_RDWR")
(define shut/rd SHUT_RD)
(define shut/wr SHUT_WR)
(define shut/rdwr SHUT_RDWR)

(define _close_socket (foreign-lambda int "closesocket" int))
(define strerror (foreign-lambda c-string "skt_strerror" int))

(define _make_socket_nonblocking
  (foreign-lambda* bool ((int fd))
    "#ifdef _WIN32\n"
    "unsigned long val = 1; C_return(ioctlsocket(fd, FIONBIO, &val) == 0);\n"
    "#else\n"
    "int val = fcntl(fd, F_GETFL, 0);"
    "if(val == -1) C_return(0);"
    "C_return(fcntl(fd, F_SETFL, val | O_NONBLOCK) != -1);\n"
    "#endif\n"))

(define select-for-read
  (foreign-lambda* int ((int fd))
    "fd_set in;
     struct timeval tm;
     int rv;
     FD_ZERO(&in);
     FD_SET(fd, &in);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, &in, NULL, NULL, &tm);
     if(rv > 0) { rv = FD_ISSET(fd, &in) ? 1 : 0; }
     C_return(rv);") )

(define select-for-write
  (foreign-lambda* int ((int fd))
    "fd_set out;
     struct timeval tm;
     int rv;
     FD_ZERO(&out);
     FD_SET(fd, &out);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, NULL, &out, NULL, &tm);
     if(rv > 0) { rv = FD_ISSET(fd, &out) ? 1 : 0; }
     C_return(rv);") )

;; On Windows, non-blocking connection errors show up in except fds.
(define select-for-write-or-except
  (foreign-lambda* int ((int fd))
    "fd_set out, exc;
     struct timeval tm;
     int rv;
     FD_ZERO(&out); FD_ZERO(&exc);
     FD_SET(fd, &out); FD_SET(fd, &exc);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, NULL, &out, &exc, &tm);
     if(rv > 0) { rv = (FD_ISSET(fd, &out) || FD_ISSET(fd, &exc)) ? 1 : 0; }
     C_return(rv);") )

(define-inline (network-error where msg . args)
  (apply ##sys#signal-hook #:network-error where msg args))
(define-inline (network-error/errno where msg . args)
  (let ((err errno))
    (##sys#update-errno) ;; Note that this may cause context switch, and wipe out errno
    (apply ##sys#signal-hook #:network-error where
           (string-append msg " - " (strerror err))
           args)))
(define-inline (network-error/errno* where err msg . args)
;;(##sys#update-errno)
  (apply ##sys#signal-hook #:network-error where
         (string-append msg " - " (strerror err))
         args))
(define-inline (socket-timeout-error where timeout so)
  (##sys#signal-hook
   #:network-timeout-error
   where "operation timed out" timeout so))

(define (block-for-timeout! where timeout fd type #!optional cleanup)  ;; #f permitted for WHERE
  ;; No exported way to simultaneously wait on either an FD or a timeout event.
  (when timeout
    (##sys#thread-block-for-timeout!
     ##sys#current-thread
     (+ (current-milliseconds) timeout)))
  (##sys#thread-block-for-i/o! ##sys#current-thread fd type)
  (##sys#thread-yield!)
  (when (##sys#slot ##sys#current-thread 13)
    (if cleanup (cleanup))
    (##sys#signal-hook
     #:network-timeout-error
     where "operation timed out" timeout fd)))

(define (get-socket-error s)
  ;; http://cr.yp.to/docs/connect.html describes alternative ways to retrieve
  ;; non-blocking socket errors.
  (define _getsockerr
    (foreign-lambda* int ((int socket))
      "int err;"
      "int optlen = sizeof(err);"
      "if (typecorrect_getsockopt(socket, SOL_SOCKET, SO_ERROR, &err, (socklen_t *)&optlen) == -1)"
      "C_return(-1);"
      "C_return(err);"))
  (let ((err (_getsockerr s)))
    (cond ((fx= err 0) #f)
          ((fx> err 0) err)
          (else
           (_close_socket s)
           (network-error/errno 'get-socket-error "unable to obtain socket error code")))))

;; Silly parsing of inet address string into host and port.
;; Note: if unparsable into host/port, return full string as hostname, and let caller deal with it.
;; If host or port is empty, returns #f for that field.
(define (parse-inet-address str)
  (let ((len (string-length str)))
    (if (= len 0)
	(values "" #f)
	(if (char=? (string-ref str 0) #\[)
	    (let ((j (string-index str #\] 1)))
	      (if j
		  (let* ((host (substring str 1 j))
			 (host (if (string=? host "") #f host)))
		    (if (= (fx+ j 1) len)
			(values host #f)      ;; bracketed address w/o port
			(if (char=? (string-ref str (fx+ j 1)) #\:)
			    (let* ((port (substring str (fx+ j 2)))
				   (port (if (string=? port "") #f port)))
			      (values host port)) ;; bracketed address w/ port
			    (values str #f))))
		  (values str #f)))
	    (let ((j (string-index str #\:)))
	      (if j
		  (let ((k (string-index str #\: (fx+ j 1))))
		    (if k
			(values str #f)   ;; a bare IPv6 address
			(let* ((host (substring str 0 j))
			       (host (if (string=? host "") #f host))
			       (port (substring str (fx+ j 1)))
			       (port (if (string=? port "") #f port)))
			  (values host port)))) ;; IPv4 address w/port
		  (values str #f)) ;; an IPv4 address sans port
	      )))))

(define-record socket fileno family type protocol)  ;; NB socket? conflicts with Unit posix
(define-inline (%socket-fileno so)
  (##sys#slot so 1))

(define-record-printer (socket s out)
  (fprintf out "#<socket fd:~S ~S ~S>"
           (socket-fileno s)
           (non-nil (integer->address-family (socket-family s)) (socket-family s))
           (non-nil (integer->socket-type (socket-type s)) (socket-type s))
           #;(non-nil (integer->protocol-type (socket-protocol s)) (socket-protocol s))
           ))

(define (socket family socktype protocol)
  (define _socket (foreign-lambda int "socket" int int int))
  (let ((s (_socket family socktype protocol)))
    (when (eq? _invalid_socket s)
      (network-error/errno 'socket "cannot create socket"
                           (non-nil (integer->address-family family) family)
                           (non-nil (integer->socket-type socktype) socktype)
                           (non-nil (integer->protocol-type protocol) protocol)))
    (make-socket s family socktype protocol)))

(define-inline (transient-network-error/errno* where err msg . args)
  (abort
   (make-composite-condition
    (make-property-condition 'exn 'location where
                              'message (string-append msg " - " (strerror err))
                             'arguments args)
    (make-property-condition 'i/o)
    (make-property-condition 'net 'errno err)
    (make-property-condition 'transient))))

(use srfi-18)

;; Stolen from sql-de-lite (itself stolen from sqlite), but modified to respect
;; actual elapsed time instead of estimated elapsed time (to mostly avoid scheduling jitter).
;; The polling intervals are not altered, only the total elapsed time.
(define busy-timeout
  (let* ((delays '#(1 2 5 10 15 20 25 25  25  50  50 100))
         (ndelay (vector-length delays)))
    (lambda (ms)
      (cond
       ((< ms 0) (error 'busy-timeout "timeout must be non-negative" ms))
       ((= ms 0) #f)
       (else
        (let ((start (current-milliseconds)))
          (lambda (so count)
            (let* ((delay (vector-ref delays (min count (- ndelay 1))))
                   (prior (- (current-milliseconds) start)))
              (let ((delay (if (> (+ prior delay) ms)
                               (- ms prior)
                               delay)))
                (cond ((<= delay 0) #f)
                      (else
                       (thread-sleep! (/ delay 1000)) ;; silly division
                       #t)))))))))))

(define-constant +largest-fixnum+ (##sys#fudge 21)) 

;; Returns a special "transient" error (exn i/o net transient) if connection failure
;; was due to refusal, network down, etc.; in which case, the same or another
;; address could be tried later. (The socket is still closed, though.)
(define (socket-connect! so saddr)
  (define _connect (foreign-lambda int "connect" int scheme-pointer int))
  (define (refused? err)
    (or (eq? err _econnrefused) (eq? err _etimedout)
        (eq? err _enetunreach) (eq? err _ehostunreach)))
  (let ((s (socket-fileno so))
        (timeout (socket-connect-timeout)))
    (unless (_make_socket_nonblocking s)
      (network-error/errno 'socket-connect! "unable to set socket to non-blocking" so))
    (when (eq? -1 (_connect s (sockaddr-blob saddr) (sockaddr-len saddr)))
      (let ((err errno))
        (if (or (eq? err _einprogress)
                (eq? err _ewouldblock))
            (begin
              (cond-expand
               (windows   ;; WINSOCK--connect failure returned in exceptfds; manually schedule
                (let ((wait (busy-timeout (or timeout +largest-fixnum+)))) ;; 12.4 days on 32bit
                  (let loop ((n 0))
                    (let ((f (select-for-write-or-except s)))
                      (cond ((eq? f -1)
                             (network-error/errno 'socket-connect! "select failed" so))
                            ((eq? f 0)
                             (if (wait so n)
                                 (loop (+ n 1))
                                 (socket-timeout-error 'socket-connect! timeout so)))
                            ;; else f=1, fall through
                            )))))
               (else  ;; POSIX--connect failure returned in writefds
                (let ((f (select-for-write s)))  ;; May be ready immediately; don't reschedule.
                  (cond ((eq? f -1)
                         (network-error/errno 'socket-connect! "select failed" so))
                        ((eq? f 0)
                         (block-for-timeout! 'socket-connect! timeout s #:output
                                             (lambda () (_close_socket s))))
                        ;; else f=1, fall through
                        ))))
              (cond ((get-socket-error s)
                     => (lambda (err)
                          (_close_socket s)
                          ((if (refused? err)
                               transient-network-error/errno*
                               network-error/errno*)
                           'socket-connect! err "cannot initiate connection"
                           so saddr)))))
            (begin
              (_close_socket s)
              ((if (refused? err)
                   transient-network-error/errno*
                   network-error/errno*)
               'socket-connect! err "cannot initiate connection" so saddr)))))
    ;; perhaps socket address should be stored in socket object
    (void)))

;; Sequentially connect to all addrinfo objects until one succeeds, as long
;; as the connection is retryable (e.g. refused, no route, or timeout).
;; Otherwise it will error out on non-recoverable errors.
;; Returns: fresh socket associated with the succeeding connection, or throws
;; an error corresponding to the last failed connection attempt.
;; Example: (socket-connect/ai (address-information "localhost" 22 type: sock/stream))
;; NB: Connection to sock/dgram will generally succeed, so to ensure TCP connection,
;;     make sure to specify sock/stream.
;; NB: On Windows XP, a 0 value for socket type will default to TCP (no matter the
;;     value of protocol).  address-information returns 0 for type and protocol when
;;     not specified.  For safety, you should always provide "type:" or specify a
;;     service name (not port).
(define (socket-connect/ai ais)
  (when (null? ais)
    (network-error 'socket-connect/ai "no addresses to connect to"))
  (let loop ((ais ais))
    (let* ((ai (car ais))
           (addr (addrinfo-address ai))
           (so (socket (addrinfo-family ai) (addrinfo-socktype ai) 0))
           (s (socket-fileno so)))
      (if (null? (cdr ais))
          (begin (socket-connect! so addr) so)
          (condition-case
           (begin (socket-connect! so addr) so)
           (e (exn i/o net timeout)
              (loop (cdr ais)))
           (e (exn i/o net transient)
              (loop (cdr ais))))))))

;; (socket-bind! s (addrinfo-address (car (address-information "127.0.0.1" service: 9112 socktype: sock/stream flags: ai/passive))))
;; ... is verbose; perhaps could be streamlined.

(define (socket-bind! so saddr)
  (define _bind (foreign-lambda int "bind" int scheme-pointer int))
  (let ((b (_bind (socket-fileno so) (sockaddr-blob saddr) (sockaddr-len saddr))))
    (if (eq? -1 b)
        (network-error/errno 'socket-bind! "cannot bind to socket" so saddr)
        (void))))

;; Listening on datagram socket throws an OS error.
(define (socket-listen! so backlog)
  (define _listen (foreign-lambda int "listen" int int))
  (let ((l (_listen (socket-fileno so) backlog)))
    (when (eq? -1 l)
      (network-error/errno 'socket-listen! "cannot listen on socket" so))))

(define (socket-close! so)
  (let ((s (socket-fileno so)))
    (when (fx= -1 (_close_socket s))
      (network-error/errno 'socket-close! "could not close socket" so))))

;; Returns a socket object representing the accepted connection.
;; Does not currently return the socket address of the remote, although it could;
;; alternatively you can get it from getpeername.
(define (socket-accept so)
  (define _accept (foreign-lambda int "accept" int c-pointer c-pointer))
  (let ((s (socket-fileno so))
        (to (socket-accept-timeout)))
    (let restart ()
      (if (eq? 1 (select-for-read s))
          (let ((s (_accept s #f #f)))
            (when (eq? -1 s)
              (network-error/errno 'socket-accept "could not accept from listener" so))
            (unless (_make_socket_nonblocking s)
              (network-error/errno 'socket-accept "unable to set socket to non-blocking" s))
            ;; iffy
            (make-socket s (socket-family so) (socket-type so) (socket-protocol so)))
          (begin
            (block-for-timeout! 'socket-accept to s #:input)
            (restart))))))

;; Returns number of bytes received.  If 0, and socket is sock/stream, peer has shut down his side.
(define (socket-receive! so buf #!optional (start 0) (end #f) (flags 0))
  (let* ((buflen (cond ((string? buf) (string-length buf))
                       ((blob? buf) (blob-size buf))
                       (else
                        (network-error 'socket-receive!
                                       "receive buffer must be a blob or a string" so))))
         (end (or end buflen)))
    (##sys#check-exact start)    
    (##sys#check-exact end)
    (##sys#check-exact flags)
    (when (or (fx< start 0)
              (fx> end buflen)
              (fx< end start))
      (network-error 'socket-receive! "receive buffer offsets out of range" start end))
    (%socket-receive! so buf start (fx- end start) flags (socket-read-timeout))))

;; Variant of socket-receive! which does not check so, buf, start, or len and which takes
;; read timeout as parameter.  Basically for use in socket ports.
(define (%socket-receive! so buf start len flags timeout)
  (define _recv_offset (foreign-lambda* int ((int s) (scheme-pointer buf) (int start)
                                             (int len) (int flags))
                         "C_return(recv(s,((char*)buf)+start,len,flags));"))
  (let ((s (%socket-fileno so)))
    (let restart ()
      (let ((n (_recv_offset s buf start len flags)))
        (cond ((eq? -1 n)
               (let ((err errno))
                 (cond ((eq? err _ewouldblock)
                        (block-for-timeout! 'socket-receive! timeout s #:input)
                        (restart))
                       (else
                        (network-error/errno* 'socket-receive! err "cannot read from socket" so)))))
              (else n))))))

(define (socket-receive-ready? so)
  (let ((f (select-for-read (socket-fileno so))))
    (when (eq? -1 f)
      (network-error/errno 'socket-receive-ready? "unable to check socket for input" so))
    (eq? 1 f)))
(define socket-accept-ready? socket-receive-ready?)

(define (socket-send! so buf #!optional (start 0) (end #f) (flags 0))
  (let* ((buflen (cond ((string? buf) (string-length buf))
                       ((blob? buf) (blob-size buf))
                       (else
                        (network-error 'socket-send!
                                       "send buffer must be a blob or a string" so))))
         (end (or end buflen)))
    (##sys#check-exact start)
    (##sys#check-exact end)
    (##sys#check-exact flags)
    (when (or (fx< start 0)
              (fx> end buflen)
              (fx< end start))
      (network-error 'socket-send! "send buffer offsets out of range" start end))
    (%socket-send! so buf start (fx- end start) flags (socket-write-timeout))))
(define (%socket-send! so buf start len flags timeout)
  (define _send_offset (foreign-lambda* int ((int s) (scheme-pointer buf) (int start)
                                             (int len) (int flags))
                         "C_return(send(s,((char*)buf)+start,len,flags));"))
  (let ((s (%socket-fileno so)))
    (let retry ((len len) (start start))
      (let ((n (_send_offset s buf start len flags)))
        (cond ((eq? -1 n)
               (let ((err errno))
                 (cond ((eq? err _ewouldblock)
                        (block-for-timeout! 'socket-send! timeout s #:output)
                        (retry len start))
                       (else
                        (network-error/errno* 'socket-send! err "cannot send to socket" so)))))
              (else n))))))

(define-foreign-variable +maximum-string-length+ int "C_HEADER_SIZE_MASK")  ;; horrible
;; NB.  Possible the chunking functionality belongs in %socket-send.  We *could* limit
;; the packet size there.
(define (%socket-send-all! so buf start slen flags timeout chunksz)
  (let ((chunksz (or chunksz +maximum-string-length+)))
    (let loop ((len slen) (start start))
      (let* ((count (fxmin chunksz len))
             (n (%socket-send! so buf start count flags timeout)))
        (if (fx< n len)
            (loop (fx- len n) (fx+ start n))
            (void))))))

;; Socket output chunk size for send-all.  For compatibility with Unit TCP; maybe not necessary.
;; If #f, attempt to send as much as possible.  Only question is whether it is safe to exceed
;; the socket send buffer size, which may (according to Microsoft pages) cause stalling until
;; delayed ACKs come back.
(define socket-send-size (make-parameter 16384))
(define socket-send-buffer-size (make-parameter #f))
;;(define socket-receive-size (make-parameter 1024))      ;;?
(define socket-receive-buffer-size (make-parameter 4096))

(define (socket-send-all! so buf #!optional (start 0) (end #f) (flags 0))
  (let* ((buflen (cond ((string? buf) (string-length buf))
                       ((blob? buf) (blob-size buf))
                       (else
                        (network-error 'socket-send-all!
                                       "send buffer must be a blob or a string" so))))
         (end (or end buflen)))
    (##sys#check-exact start)
    (##sys#check-exact end)
    (##sys#check-exact flags)
    (when (or (fx< start 0)
              (fx> end buflen)
              (fx< end start))
      (network-error 'socket-send-all! "send buffer offsets out of range" start end))
    (%socket-send-all! so buf start (fx- end start) flags
                       (socket-write-timeout)
                       (socket-send-size))))

;; Shutdown socket.  If socket is not connected, silently ignore the error, because
;; the peer may have already initiated shutdown.  That behavior should perhaps be configurable.
(define (socket-shutdown! so how)  ;; how: shut/rd, shut/wr, shut/rdwr
  (define _shutdown (foreign-lambda int "shutdown" int int))
  (when (eq? -1 (_shutdown (socket-fileno so) how))
    (let ((err errno))
      (unless (eq? err _enotconn)
        (network-error/errno* 'socket-shutdown! err "unable to shutdown socket" so how))))
  (void))

(define (socket-name so)   ;; a legacy name
  (define _free (foreign-lambda void "C_free" c-pointer))
  (let-location ((len int))
    (let ((sa (_getsockname (socket-fileno so) (location len))))
      (if (fx= sa -1)
          (network-error/errno 'socket-name "unable to get socket name" so)
          (let ((addr (sa->sockaddr sa len)))
            (_free sa)
            addr)))))

(define (socket-peer-name so)
  (define _free (foreign-lambda void "C_free" c-pointer))
  (let-location ((len int))
    (let ((sa (_getpeername (socket-fileno so) (location len))))
      (if (fx= sa -1)
          (network-error/errno 'socket-peer-name "unable to get socket peer name" so)
          (let ((addr (sa->sockaddr sa len)))
            (_free sa)
            addr)))))

(define _getsockname
  (foreign-lambda* c-pointer ((int s) ((c-pointer int) len))
    "struct sockaddr_storage *ss;"
    "ss = (struct sockaddr_storage *)C_malloc(sizeof(*ss));"
    "*len = sizeof(*ss);"
    "if (getsockname(s, (struct sockaddr *)ss, (socklen_t *)len) != 0) C_return(NULL);"
    "C_return(ss);"))
(define _getpeername
  (foreign-lambda* c-pointer ((int s) ((c-pointer int) len))
    "struct sockaddr_storage *ss;"
    "ss = (struct sockaddr_storage *)C_malloc(sizeof(*ss));"
    "*len = sizeof(*ss);"
    "if (getpeername(s, (struct sockaddr *)ss, (socklen_t *)len) != 0) C_return(NULL);"
    "C_return(ss);"))


;;; socket options

;; FIXME: Temporary for tcp6 egg
(define (set-socket-reuseaddr! so flag)
  (when (eq? -1 ((foreign-lambda* int ((int socket) (bool flag)) 
                   "flag = flag ? 1 : 0;
                    C_return(setsockopt(socket, SOL_SOCKET, SO_REUSEADDR, (const char *)&flag, sizeof(flag)));") 
                 so flag))
    (network-error/errno 'tcp-listen "error setting SO_REUSEADDR" so)))

;; FIXME: Temporary for tcp6 egg
(define (set-socket-v6only! so flag)
  (when (eq? -1 ((foreign-lambda* int ((int socket) (bool flag))
                   "#ifdef IPV6_V6ONLY\n"
                   "flag = flag ? 1 : 0;"
                   "C_return(setsockopt(socket, IPPROTO_IPV6, IPV6_V6ONLY, (const char *)&flag, sizeof(flag)));\n"
                   "#else\n"
                   "C_return(0);\n" ;; silently fail
                   "#endif\n")
                 s (tcp-bind-ipv6-only)))
    (network-error/errno 'tcp-listen "error setting IPV6_V6ONLY" so)))

;;; ports

(define-inline (socket-port-data p)
  (##sys#check-port p 'socket-abandon-port!)
  (unless (eq? (##sys#slot p 7)
	       'socket6)            ;; port data has no header (right now); use port type id
    (error 'socket-port-data "argument is not a socket port" p))
  (##sys#port-data p))
(define-inline (%socket-port-data-socket data)            (##sys#slot data 0))
(define-inline (%socket-port-data-input-abandoned? data)  (##sys#slot data 1))
(define-inline (%socket-port-data-output-abandoned? data) (##sys#slot data 2))

(define (socket-i/o-port->socket p)
  (%socket-port-data-socket (socket-port-data p)))

(define socket-i/o-ports
    (lambda (so)
      (let* ((fd (socket-fileno so))
             (input-buffer-size (socket-receive-buffer-size))
	     (buf (make-string input-buffer-size))
	     (data (vector so #f #f buf 0))  ;; Native socket ports have fd in slot 0.
	     (buflen 0)
	     (bufindex 0)
	     (iclosed #f) 
	     (oclosed #f)
	     (outbufsize (socket-send-buffer-size))
	     (outbuf (and outbufsize (fx> outbufsize 0)
			  (make-string outbufsize)))
	     (outbufindex 0)
	     (tmr (socket-read-timeout))
	     (tmw (socket-write-timeout))
	     (output-chunk-size (socket-send-size))
	     (read-input
	      (lambda ()
		(let ((n (%socket-receive! so buf 0 input-buffer-size 0 tmr)))
		  (set! buflen n)
		  (##sys#setislot data 4 n)
		  (set! bufindex 0))))
	     (in
	      (make-input-port
	       (lambda ()
		 (when (fx>= bufindex buflen)
		   (read-input))
		 (if (fx>= bufindex buflen)
		     #!eof
		     (let ((c (##core#inline "C_subchar" buf bufindex)))
		       (set! bufindex (fx+ bufindex 1))
		       c) ) )
	       (lambda ()
		 (or (fx< bufindex buflen)
		     (socket-receive-ready? so)))
	       (lambda ()
		 (unless iclosed
		   (set! iclosed #t)
		   (unless (%socket-port-data-input-abandoned? data)       ;; Skip this for dgram?
		     (socket-shutdown! so shut/rd))  ;; Must not error if peer has shutdown.
		   (when oclosed
		     (socket-close! so))))
	       (lambda ()
		 (when (fx>= bufindex buflen)
		   (read-input))
		 (if (fx< bufindex buflen)
		     (##core#inline "C_subchar" buf bufindex)
		     #!eof))
	       (lambda (p n dest start)	; read-string!
		 (let loop ((n n) (m 0) (start start))
		   (cond ((eq? n 0) m)
			 ((fx< bufindex buflen)
			  (let* ((rest (fx- buflen bufindex))
				 (n2 (if (fx< n rest) n rest)))
			    (##core#inline "C_substring_copy" buf dest bufindex (fx+ bufindex n2) start)
			    (set! bufindex (fx+ bufindex n2))
			    (loop (fx- n n2) (fx+ m n2) (fx+ start n2)) ) )
			 (else
			  (read-input)
			  (if (eq? buflen 0) 
			      m
			      (loop n m start) ) ) ) ) )
	       (lambda (p limit)	; read-line
		 (let loop ((str #f)
			    (limit (or limit (##sys#fudge 21))))
		   (cond ((fx< bufindex buflen)
			  (##sys#scan-buffer-line
			   buf 
			   (fxmin buflen limit)
			   bufindex
			   (lambda (pos2 next)
			     (let* ((len (fx- pos2 bufindex))
				    (dest (##sys#make-string len)))
			       (##core#inline "C_substring_copy" buf dest bufindex pos2 0)
			       (set! bufindex next)
			       (cond ((eq? pos2 limit) ; no line-terminator, hit limit
				      (if str (##sys#string-append str dest) dest))
				     ((eq? pos2 next) ; no line-terminator, hit buflen
				      (read-input)
				      (if (fx>= bufindex buflen)
					  (or str "")
					  (loop (if str (##sys#string-append str dest) dest)
						(fx- limit len)) ) )
				     (else 
				      (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1))
				      (if str (##sys#string-append str dest) dest)) ) ) ) ) )
			 (else
			  (read-input)
			  (if (fx< bufindex buflen)
			      (loop str limit)
			      #!eof) ) ) ) )
	       ;; (lambda (p)		; read-buffered
	       ;;   (if (fx>= bufindex buflen)
	       ;;       ""
	       ;;       (let ((str (##sys#substring buf bufpos buflen)))
	       ;;         (set! bufpos buflen)
	       ;;         str)))
	       ) )
	     (output
	      (lambda (str off len)
		(%socket-send-all! so str off len 0 tmw output-chunk-size)))
	     (out
	      (make-output-port
	       (if outbuf
		   (lambda (s)
                     ;; This sends the whole existing buffer + string as soon as it exceeds
                     ;; the buffer size.  That is useful to buffer small amounts of data
                     ;; (bufsz < chunksz).  We could also send only in bufsz increments.
                     ;; That is useful when bufsz > chunksz and bufsz is a multiple (I think).
                     ;; Also may make sense when sending UDP to guarantee packets are always
                     ;; fixed size until an explicit flush.  Of course if you have that requirement
                     ;; I suspect you will need to construct packets/strings yourself to ensure
                     ;; the last one is padded. (?)

                     ;; Modified from Unit TCP.  No longer does string-appends to build buffer;
                     ;; instead writes into static buffer until exhausted, with a single
                     ;; string-append at end if exceeded buffer space.
                     (let ((olen (fx+ (##sys#size s) outbufindex)))
                       (cond ((fx= (##sys#size s) 0))
                             ((fx< olen outbufsize)
                              (##core#inline "C_substring_copy" s outbuf 0 (##sys#size s) outbufindex)
                              (set! outbufindex olen))
                             ((fx= olen outbufsize)
                              (##core#inline "C_substring_copy" s outbuf 0 (##sys#size s) outbufindex)
                              (output outbuf 0 outbufsize)
                              (set! outbufindex 0))
                             (else
			      ;; Optimizations: If empty buffer, no string-append required.
			      ;; Future opts: Can probably do smaller string appends of one
			      ;; chunk for chunk alignment, then write rest out.  Until then,
			      ;; you can flush the buffer before a big write.
			      (let* ((slop (fxmod olen outbufsize))
				     (end (fx- olen slop)))
				(print `(slop ,slop end ,end))
				(let ((s (if (fx= outbufindex 0)
					     s
					     (##sys#string-append
					      (substring outbuf 0 outbufindex) s))))
				  (print `(s ,s))
				  (output s 0 end)
				  (when (fx> slop 0)
				    (print `(slopping))
				    (##core#inline "C_substring_copy" s outbuf end olen 0))
				  (set! outbufindex slop))))))
                     (void))
		   (lambda (s) 
		     (when (fx> (##sys#size s) 0)
		       (output s 0 (##sys#size s))) ) )
	       (lambda ()
		 (unless oclosed
		   (set! oclosed #t)
		   (when (and outbuf (fx> outbufindex 0))
                     (output outbuf 0 outbufindex)
		     (set! outbufindex 0))
                   ;; Note some odd closesocket() behavior with discarded output at:
                   ;; http://msdn.microsoft.com/en-us/library/ms738547 (v=vs.85).aspx
		   (unless (%socket-port-data-output-abandoned? data)
		     (socket-shutdown! so shut/wr))
		   (when iclosed
		     (socket-close! so))))
	       (and outbuf
		    (lambda ()
		      (when (fx> outbufindex 0)
			(output outbuf 0 outbufindex)
			(set! outbufindex 0) ) ) ) ) ) )
	(##sys#setslot in 3 "(socket)")
	(##sys#setslot out 3 "(socket)")
	(##sys#setslot in 7 'socket6)
	(##sys#setslot out 7 'socket6)
	(##sys#set-port-data! in data)
	(##sys#set-port-data! out data)
	(values in out) ) ) ) 

(define (socket-abandon-port! p)
  (let ((d (socket-port-data p)))
    (if (input-port? p)
	(##sys#setislot d 1 #t)
	(##sys#setislot d 2 #t))))   ;; Note: polarity is reversed from unit tcp
