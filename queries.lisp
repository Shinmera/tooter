#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter)

;;; Accounts

(defmethod find-account ((client client) (id integer))
  (decode-account (query client (format NIL "/api/v1/accounts/~a" id))))

(defmethod verify-credentials ((client client))
  (decode-account (query client "/api/v1/accounts/verify_credentials")))

(defmethod update-credentials ((client client) &key display-name note avatar header (locked NIL l-p) fields)
  (check-type display-name (or null string))
  (check-type node (or null string))
  (check-type avatar (or null pathname))
  (check-type header (or null pathname))
  (check-type fields list)
  (decode-account (apply #'submit client "/api/v1/accounts/update_credentials"
                         :display-name display-name
                         :note note
                         :avatar avatar
                         :header header
                         :locked (coerce-boolean locked l-p)
                         (loop for i from 0
                               for (key . val) in fields
                               collect (format NIL "fields_attributes[~a][name]" i)
                               collect key
                               collect (format NIL "fields_attributes[~a][value]" i)
                               collect val))))

(defmethod get-followers ((client client) (id integer) &key max-id since-id limit)
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/accounts/~a/followers" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod get-followers ((client client) (account account) &rest args)
  (apply #'get-followers client (id account) args))

(defmethod get-following ((client client) (id integer) &key max-id since-id limit)
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/accounts/~a/following" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod get-following ((client client) (account account) &rest args)
  (apply #'get-following client (id account) args))

(defmethod get-statuses ((client client) (id integer) &key (only-media NIL o-p) (pinned NIL p-p) (exclude-replies NIL e-p) max-id since-id limit)
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-status (query client (format NIL "/api/v1/accounts/~a/statuses" id)
                        :only-media (coerce-boolean only-media o-p)
                        :pinned (coerce-boolean pinned p-p)
                        :exclude-replies (coerce-boolean exclude-replies e-p)
                        :max-id max-id
                        :since-id since-id
                        :limit limit)))

(defmethod get-statuses ((client client) (account account) &rest args)
  (apply #'statuses client (id account) args))

(defmethod follow ((client client) (id integer))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/follow" id))))

(defmethod follow ((client client) (account account))
  (if (string= (username account) (account account))
      (follow client (id account))
      (follow-remote client (account-name account))))

(defmethod unfollow ((client client) (id integer))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/unfollow" id))))

(defmethod unfollow ((client client) (account account))
  (unfollow client (id account)))

(defmethod block ((client client) (id integer))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/block" id))))

(defmethod block ((client client) (account account))
  (block client (id account)))

(defmethod unblock ((client client) (id integer))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/unblock" id))))

(defmethod unblock ((client client) (account account))
  (unblock client (id account)))

(defmethod mute ((client client) (id integer) &key (notifications T n-p))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/mute" id)
                               :notifications (coerce-boolean notifications n-p))))

(defmethod mute ((client client) (account account) &rest args)
  (apply #'mute client (id account) args))

(defmethod unmute ((client client) (id integer))
  (decode-relationship (submit client (format NIL "/api/v1/accounts/~a/unmute" id))))

(defmethod unmute ((client client) (account account))
  (unblock client (id account)))

(defmethod relationships ((client client) (ids cons))
  (decode-relationship (query client "/api/v1/accounts/relationships"
                              :id (loop for id in ids
                                        collect (typecase id
                                                  (account (id id))
                                                  (T id))))))

(defmethod relationships ((client client) (account account))
  (relationships client (list (id account))))

(defmethod search-accounts ((client client) query &key (limit 40) (following NIL f-p))
  (check-type query string)
  (check-type limit (or null (integer 0)))
  (decode-account (query client "/api/v1/accounts/search"
                         :q query
                         :limit limit
                         :following (coerce-boolean following f-p))))

;;; Blocks

(defmethod blocks ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client "/api/v1/blocks"
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod blocked-domains ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (query client "/api/v1/domain_blocks"
         :max-id max-id
         :since-id since-id
         :limit limit))

(defmethod block ((client client) (domain string))
  (submit client "/api/v1/domain_blocks"
          :domain domain)
  T)

(defmethod unblock ((client client) (domain string))
  (submit client "/api/v1/domain_blocks"
          :http-method :delete
          :domain domain)
  T)

;;; Favourites

(defmethod favourites ((client client) &key max-id since-id (limit 20))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-status (query client "/api/v1/favourites"
                        :max-id max-id
                        :since-id since-id
                        :limit limit)))

;;; Follow Requests

(defmethod follow-requests ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client "/api/v1/follow_requests"
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod accept-request ((client client) (id integer))
  (submit client (format NIL "/api/v1/follow_requests/~a/authorize" id)))

(defmethod accept-request ((client client) (account account))
  (accept-request client (id account)))

(defmethod reject-request ((client client) (id integer))
  (submit client (format NIL "/api/v1/follow_requests/~a/reject" id)))

(defmethod reject-request ((client client) (account account))
  (reject-request client (id account)))

;;; Follows

(defmethod follow-remote ((client client) (uri string))
  (decode-account (submit client "/api/v1/follows"
                          :uri uri)))

(defmethod follow-remote ((client client) (account account))
  (follow-remote client (account-name account)))

;;; Instances

(defmethod instance ((client client))
  (decode-instance (query client "/api/v1/instance")))

(defmethod emojis ((client client))
  (decode-emoji (query client "/api/v1/custom_emojis")))

;;; Lists

(defmethod user-lists ((client client) (id integer))
  (decode-user-list (query client (format NIL "/api/v1/accounts/~a/lists" id))))

(defmethod user-lists ((client client) (account account))
  (user-lists client (id account)))

(defmethod user-lists ((client client) (id (eql T)))
  (decode-user-list (query client "/api/v1/lists")))

(defmethod user-list-accounts ((client client) (id integer) &key max-id since-id limit)
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/lists/~a/accounts" id
                                        :max-id max-id
                                        :since-id since-id
                                        :limit (or limit 0)))))

(defmethod user-list-accounts ((client client) (user-list user-list) &rest args)
  (apply #'user-list-accounts client (id user-list) args))

;; Convenience.
(defmethod (setf user-list-accounts) (accounts (client client) (id integer))
  (let* ((accounts (loop for account in accounts
                         collect (etypecase account
                                   (integer account)
                                   (account (id account)))))
         (existing (mapcar #'id (user-list-accounts client id)))
         (to-remove (set-difference existing accounts))
         (to-add (set-difference accounts existing)))
    (when to-remove (remove-user-list-accounts client id to-remove))
    (when to-add (add-user-list-accounts client id to-add))
    accounts))

(defmethod find-list ((client client) (id integer))
  (decode-user-list (query client (format NIL "/api/v1/lists/~a" id))))

(defmethod make-user-list ((client client) title)
  (decode-user-list (submit client "/api/v1/lists"
                            :title title)))

(defmethod update-user-list ((client client) (id integer) &key title)
  (check-type title string)
  (decode-user-list (submit client (format NIL "/api/v1/lists/~a" id)
                            :http-method :put
                            :title title)))

(defmethod update-user-list ((client client) (user-list user-list) &rest args)
  (apply #'update-user-list client (id user-list) args))

(defmethod delete-user-list ((client client) (id integer))
  (submit client (format NIL "/api/v1/lists/~a" id)
          :http-method :delete))

(defmethod add-user-list-accounts ((client client) (id integer) accounts)
  (submit client (format NIL "/api/v1/lists/~a/accounts" id)
          :account-ids (loop for account in accounts
                             collect (etypecase account
                                       (integer account)
                                       (account (id account)))))
  T)

(defmethod add-user-list-accounts ((client client) (user-list user-list) accounts)
  (add-user-list-accounts client (id user-list) accounts))

(defmethod remove-user-list-accounts ((client client) (id integer) accounts)
  (submit client (format NIL "/api/v1/lists/~a/accounts" id)
          :http-method :delete
          :account-ids (loop for account in accounts
                             collect (etypecase account
                                       (integer account)
                                       (account (id account)))))
  T)

(defmethod remove-user-list-accounts ((client client) (user-list user-list) accounts)
  (remove-user-list-accounts client (id user-list) accounts))

;;; Media

(defmethod make-media ((client client) file &key description focus)
  (check-type description (or null string))
  (check-type focus (or null cons))
  (decode-attachment (submit client "/api/v1/media"
                             :file file
                             :description description
                             :focus (when focus (format NIL "~f,~f" (car focus) (cdr focus))))))

(defmethod update-media ((client client) id &key description focus)
  (check-type description (or null string))
  (check-type focus (or null cons))
  (decode-attachment (submit client (format NIL "/api/v1/media/~a" id)
                             :description description
                             :focus (when focus (format NIL "~f,~f" (car focus) (cdr focus))))))

(defmethod update-media ((client client) (attachment attachment) &rest args)
  (apply #'update-media client (id attachment) args))

;;; Mutes

(defmethod mutes ((client client) &key max-id since-id (limit 40))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  ;; FIXME Link headers
  (decode-account (query client "/api/v1/mutes"
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

;;; Notifications

(defmethod notifications ((client client) &key max-id since-id (limit 15) exclude-types)
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (check-type exclude-types list)
  (decode-notification (query client "/api/v1/notifications"
                              :max-id max-id
                              :since-id since-id
                              :limit limit
                              :exclude-types (loop for type in exclude-types
                                                   collect (ecase type
                                                             (:follow "follow")
                                                             (:favourite "favourite")
                                                             (:reblog "reblog")
                                                             (:mention "mention"))))))

(defmethod find-notification ((client client) (id integer))
  (decode-notification (query client (format NIL "/api/v1/notifications/~a" id))))

(defmethod delete-notification ((client client) (all (eql T)))
  (submit client "/api/v1/notifications/clear")
  T)

(defmethod delete-notification ((client client) (id integer))
  (submit client "/api/v1/notifications/dismiss"
          :id id)
  T)

(defmethod delete-notification ((client client) (notification notification))
  (delete-notification client (id notification)))

(defmethod make-subscription ((client client) endpoint public-key secret &key alerts)
  (check-type endpoint string)
  (check-type public-key string)
  (check-type secret string)
  (check-type alerts list)
  (decode-push-subscription (apply #'submit client "/api/v1/push/subscription"
                                   "subscription[endpoint]" endpoint
                                   "subscription[keys][p256dh]" public-key
                                   "subscription[keys][auth]" secret
                                   (loop for alert in alerts
                                         collect (ecase alert
                                                   (:follows "data[alerts][follow]")
                                                   (:favourites "data[alerts][favourite]")
                                                   (:reblogs "data[alerts][reblogs]")
                                                   (:mentions "data[alerts][mentions]"))
                                         collect "true"))))

(defmethod subscription ((client client))
  (decode-push-subscription (query client "/api/v1/push/subscription")))

(defmethod delete-subscription ((client client))
  (submit client "/api/v1/push/subscription"
          :http-method :delete))

;;; Reports

(defmethod reports ((client client))
  (decode-report (query client "/api/v1/reports")))

(defmethod report ((client client) (id integer) &key statuses comment)
  (check-type statuses list)
  (check-type comment string)
  (decode-report (submit client "/api/v1/reports"
                         :account-id id
                         :status-ids (loop for status in statuses
                                           collect (etypecase status
                                                     (integer status)
                                                     (status (id status))))
                         :comment comment)))

(defmethod report ((client client) (account account) &rest args)
  (apply #'report client (id account) args))

;;; Search

(defmethod results ((client client) query &key (resolve NIL r-p))
  (decode-results (query client "/api/v2/search"
                         :q query
                         :resolve (coerce-boolean resolve r-p))))

;;; Statuses

(defmethod find-status ((client client) (id integer))
  (decode-status (query client (format NIL "/api/v1/statuses/~a" id))))

(defmethod context ((client client) (id integer))
  (decode-context (query client (format NIL "/api/v1/statuses/~a/context" id))))

(defmethod context ((client client) (status status))
  (context client (id status)))

(defmethod card ((client client) (id integer))
  (decode-card (query client (format NIL "/api/v1/statuses/~a/context" id))))

(defmethod card ((client client) (status status))
  (card client (id status)))

(defmethod rebloggers ((client client) (id integer) &key max-id since-id (limit 40))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/statuses/~a/reblogged_by" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod rebloggers ((client client) (status status) &rest args)
  (apply #'rebloggers client (id status) args))

(defmethod favouriters ((client client) (id integer) &key max-id since-id (limit 40))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-account (query client (format NIL "/api/v1/statuses/~a/favourited_by" id)
                         :max-id max-id
                         :since-id since-id
                         :limit limit)))

(defmethod favouriters ((client client) (status status) &rest args)
  (apply #'favouriters client (id status) args))

(defmethod make-status ((client client) status &key in-reply-to media (sensitive NIL s-p) spoiler-text visibility language)
  ;; FIXME: Idempotency
  (flet ((ensure-media-id (media)
           (etypecase media
             (integer media)
             (attachment (id media)))))
    (decode-status (submit client "/api/v1/statuses"
                           :status status
                           :in-reply-to-id (etypecase in-reply-to
                                             (integer in-reply-to)
                                             (status (id in-reply-to))
                                             (null NIL))
                           :media-ids (typecase media
                                        (null NIL)
                                        (cons (mapcar #'ensure-media-id media))
                                        (T (list (ensure-media-id media))))
                           :sensitive (coerce-boolean sensitive s-p)
                           :spoiler-text spoiler-text
                           :visibility (ecase visibility
                                         ((NIL) NIL)
                                         (:direct "direct")
                                         (:private "private")
                                         (:unlisted "unlisted")
                                         (:public "public"))
                           :language (when language (string language))))))

(defmethod delete-status ((client client) (id integer))
  (submit client (format NIL "/api/v1/statuses/~a" id)
          :http-method :delete)
  T)

(defmethod delete-status ((client client) (status status))
  (delete-status client (id status)))

(defmethod reblog ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/reblog" id))))

(defmethod reblog ((client client) (status status))
  (reblog client (id status)))

(defmethod unreblog ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unreblog" id))))

(defmethod unreblog ((client client) (status status))
  (unreblog client (id status)))

(defmethod favourite ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/favourite" id))))

(defmethod favourite ((client client) (status status))
  (favourite client (id status)))

(defmethod unfavourite ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unfavourite" id))))

(defmethod unfavourite ((client client) (status status))
  (unfavourite client (id status)))

(defmethod pin ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/pin" id))))

(defmethod pin ((client client) (status status))
  (pin client (id status)))

(defmethod unpin ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unpin" id))))

(defmethod unpin ((client client) (status status))
  (unpin client (id status)))

(defmethod mute-conversation ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/mute" id))))

(defmethod mute-conversation ((client client) (status status))
  (mute-conversation client (id status)))

(defmethod mute ((client client) (status status) &key)
  (mute-conversation client (id status)))

(defmethod unmute ((client client) (id integer))
  (decode-status (submit client (format NIL "/api/v1/statuses/~a/unmute" id))))

(defmethod unmute ((client client) (status status))
  (unmute client (id status)))

;;; Timelines
(defun %timeline (client url &key (local NIL l-p) (only-media NIL o-p) max-id since-id (limit 20))
  (check-type max-id (or null (integer 0)))
  (check-type since-id (or null (integer 0)))
  (check-type limit (or null (integer 0)))
  (decode-status (query client (format NIL "/api/v1/timelines/~a" url)
                        :local (coerce-boolean local l-p)
                        :only-media (coerce-boolean only-media o-p)
                        :max-id max-id
                        :since-id since-id
                        :limit limit)))

(defgeneric timeline (client kind &key local only-media max-id since-id))

(defmethod timeline ((client client) (kind (eql :home)) &rest args)
  (apply #'%timeline client "home" args))

(defmethod timeline ((client client) (kind (eql :public)) &rest args)
  (apply #'%timeline client "public" args))

(defmethod timeline ((client client) (tag string) &rest args)
  (apply #'%timeline client (format NIL "tag/~a" tag) args))

(defmethod timeline ((client client) (id integer) &rest args)
  (apply #'%timeline client (format NIL "list/~a" id) args))

(defmethod timeline ((client client) (tag tag) &rest args)
  (apply #'timeline client (name tag) args))

(defmethod timeline ((client client) (user-list user-list) &rest args)
  (apply #'timeline client (id user-list) args))

;;; Trends

(defmethod trends ((client client))
  (decode-tag (query client "/api/v1/trends")))
