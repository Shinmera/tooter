#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter)

(defun %request (uri parameters headers method content-type)
  (let ((drakma:*text-content-types* '(("application" . "json"))))
    (drakma:http-request uri :method method
                             :parameters parameters
                             :content-type content-type
                             :additional-headers headers
                             :external-format-out :utf-8
                             :want-stream T)))

(define-condition request-failed (error)
  ((code :initarg :code :reader code)
   (data :initarg :data :reader data)
   (message :initarg :message :reader message))
  (:report (lambda (c s) (format s "Mastodon request failed with code ~d~@[:~%  ~a~]"
                                 (code c) (message c)))))

(defun request (uri &key parameters headers (method :get) (content-type "application/x-www-form-urlencoded"))
  (multiple-value-bind (stream code headers)
      (%request uri parameters headers method content-type)
    (let ((data (unwind-protect
                     (yason:parse stream)
                  (close stream))))
      (if (= 200 code)
          (values data headers)
          (error 'request-failed :code code
                                 :data data
                                 :message (or (getj data :error-description)
                                              (getj data :error)))))))

(defclass client ()
  ((base :initarg :base :accessor base)
   (key :initarg :key :accessor key)
   (secret :initarg :secret :accessor secret)
   (access-token :initarg :access-token :accessor access-token)
   (name :initarg :name :accessor name)
   (redirect :initarg :redirect :accessor redirect)
   (scopes :initarg :scopes :accessor scopes)
   (website :initarg :website :accessor website)
   (account :initform NIL :accessor account))
  (:default-initargs
   :base (error "BASE required.")
   :key NIL
   :secret NIL
   :access-token NIL
   :name (error "NAME required.")
   :redirect "urn:ietf:wg:oauth:2.0:oob"
   :scopes '(:read :write :follow)
   :website NIL))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a ~a" (name client) (base client))))

(defmethod account ((client client))
  (let ((account (slot-value client 'account)))
    (or account
        (verify-credentials client))))

(defmethod make-load-form ((client client) &optional env)
  (declare (ignore env))
  `(make-instance ',(type-of client)
                  :base ,(base client)
                  :key ,(key client)
                  :secret ,(secret client)
                  :access-token ,(access-token client)
                  :name ,(name client)
                  :redirect ,(redirect client)
                  :scopes (list ,@(scopes client))
                  :website ,(website client)))

(defmethod default-headers ((client client) &key idempotency-key)
  (remove nil (list
               (when (access-token client)
                 `("Authorization" . ,(format NIL "Bearer ~a" (access-token client))))
               (when idempotency-key
                 `("Idempotency-Key" . idempotency-key)))))

(defmethod query ((client client) endpoint &rest parameters)
  (let ((method (or (getf parameters :http-method) :get)))
    (remf parameters :http-method)
    (request (format NIL "~a~a" (base client) endpoint)
             :parameters (param-plist->alist parameters)
             :method method
             :content-type "application/x-www-form-urlencoded"
             :headers (default-headers client))))

(defmethod submit ((client client) endpoint &rest parameters)
  (let ((method (or (getf parameters :http-method) :post))
        (idempotency-key (getf parameters :idempotency-key)))
    (remf parameters :http-method)
    (remf parameters :idempotency-key)
    (request (format NIL "~a~a" (base client) endpoint)
             :parameters (param-plist->alist parameters)
             :method method
             :content-type "multipart/form-data"
             :headers (default-headers client :idempotency-key idempotency-key))))

(defmethod register ((client client))
  (let ((data (submit client "/api/v1/apps"
                      :client-name (name client)
                      :redirect-uris (redirect client)
                      :scopes (format NIL "~{~(~a~)~^ ~}" (scopes client))
                      :website (website client))))
    (setf (key client) (getj data :client-id))
    (setf (secret client) (getj data :client-secret))
    (values client (key client) (secret client))))

(defmethod authorize ((client client) &optional authorization-code)
  (unless (and (key client) (secret client))
    (register client))
  (cond (authorization-code
         (let ((data (submit client "/oauth/token"
                             :client-id (key client)
                             :grant-type "authorization_code"
                             :code authorization-code
                             :redirect-uri (redirect client)
                             :client-id (key client)
                             :client-secret (secret client))))
           (setf (access-token client) (getj data :access-token))
           (values client (access-token client))))
        (T
         (values NIL
                 (make-url (format NIL "~a/oauth/authorize" (base client))
                           :scope (format NIL "~{~(~a~)~^ ~}" (scopes client))
                           :response-type "code"
                           :redirect-uri (redirect client)
                           :client-id (key client))))))
