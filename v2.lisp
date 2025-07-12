(in-package #:org.shirakumo.tooter)

(defclass v2:client (client) ())

(defmethod filters ((client v2:client))
  (decode-filter (query client "/api/v2/filters")))

(defmethod filter ((client v2:client) (id string))
  (decode-filter (query client (format NIL "/api/v2/filters/~a" id))))

(defmethod filter ((client v2:client) (filter filter))
  (filter client (id filter)))

(defgeneric check-filter-action (object value))

(defmethod check-filter-action ((object v2:client) value)
  (assert (member value '("warn" "hide") :test #'string=)))

(defmethod create-filter ((client v2:client) title context
                          &key expires-in
                               (filter-action "hide")
                               (fields '()))
  (check-filter-context context)
  (check-filter-action client filter-action)
  (assert (stringp title))
  (assert (consp context))
  (decode-filter (apply #'submit
                        client
                        "/api/v2/filters"
                        :title  title
                        (encode-filter-context context)
                        :filter-action filter-action
                        :expires-in expires-in
                        (loop for i from 0
                              for (key . val) in fields
                              collect (format NIL "fields_attributes[~a][keyword]" i)
                              collect key
                              collect (format NIL "fields_attributes[~a][whole_word]" i)
                              collect val))))

(defun make-update-filter-field (id keyword &key (whole-word nil) (destroy nil))
  (list id keyword whole-word destroy))

(defmethod update-filter ((client v2:client) id title context
                          &key (filter-action "hide") expires-in (fields '()))
  (assert (stringp id))
  (assert (stringp title))
  (check-filter-context context)
  (check-filter-action client filter-action)
  (decode-filter (apply #'submit
                        client
                        (format NIL "/api/v2/filters/~a" id)
                        :http-method :put
                        :id id
                        :title title
                        :filter-action filter-action
                        :expires-in expires-in
                        (loop for i from 0
                              for attribute in fields
                              collect (format NIL "keyword_attributes[~a][keyword]" i)
                              collect (first attribute)
                              collect (format NIL "keyword_attributes[~a][whole_word]" i)
                              collect (second attribute)
                              collect (format NIL "keyword_attributes[~a][id]" i)
                              collect (third attribute)
                              collect (format NIL "keyword_attributes[~a][_destroy]" i)
                              collect (fourth attribute))
                        (encode-filter-context context))))

(defmethod delete-filter ((client v2:client) id)
  (assert (stringp id))
  (submit client
          (format NIL "/api/v2/filters/~a" id)
          :http-method :delete))

(defmethod find-filter ((client v2:client) id)
  (assert (stringp id))
  (decode-filter (query client (format NIL "/api/v2/filters/~a" id))))

(defmethod filter-keywords ((client v2:client) filter-id)
  (assert (stringp filter-id))
  (decode-filter-keyword (query client (format NIL "/api/v2/filters/~a/keywords" filter-id))))

(defmethod add-filter-keyword ((client v2:client) filter-id keyword &key (whole-word NIL w-p))
  (assert (stringp filter-id))
  (assert (stringp keyword))
  (decode-filter-keyword (submit client
                                 (format NIL "/api/v2/filters/~a/keywords" filter-id)
                                 :keyword keyword
                                 "whole_word" (coerce-boolean whole-word w-p))))

(defmethod remove-filter-keyword ((client v2:client) filter-keyword-id)
  (assert (stringp filter-keyword-id))
  (query client
         (format NIL "/api/v2/filters/keywords/~a" filter-keyword-id)
         :http-method :delete))

(defmethod instance ((client v2:client))
  (decode-instance (query client "/api/v2/instance")))

(defmethod grouped-notifications ((client v2:client)
                                  &key max-id
                                       min-id
                                       since-id
                                       (limit 15)
                                       exclude-types
                                       types
                                       account-id
                                       (expand-accounts :full)
                                       grouped-types
                                       include-filtered)
  (check-type max-id (or null string))
  (check-type since-id (or null string))
  (check-type limit (or null (integer 0)))
  (check-type exclude-types list)
  (assert (member expand-accounts '(:full :partial-avatars)))
  (with-pagination-return (decode-grouped-notifications-results)
    (query client
           "/api/v2/notifications"
           :max-id max-id
           :min-id min-id
           :since-id since-id
           :limit limit
           :types
           (loop for type in types
                 collect (encode-notification-type type))
           :exclude-types
           (loop for type in exclude-types
                 collect (encode-notification-type type))
           :account-id account-id
           :expand-accounts expand-accounts
           :grouped-types
           (loop for type in grouped-types
                 collect
                 (encode-grouped-notification-type type))
           :include-filtered include-filtered)))

(defmethod find-grouped-notification ((client v2:client) (group-key string))
  (decode-grouped-notifications-results (query client
                                               (format NIL
                                                       "/api/v2/notifications/~a"
                                                       group-key))))

(defmethod delete-grouped-notification ((client v2:client) (group-key string))
  (submit client (format nil "/api/v2/notifications/~a/dismiss" group-key))
  T)

(defmethod fetch-notification-policy ((client v2:client))
  (decode-notification-policy (query client "/api/v2/notifications/policy")))

(defmethod update-notification-policy ((client v2:client)
                                       &key
                                         (for-not-following :accept)
                                         (for-not-followers :accept)
                                         (for-new-accounts :accept)
                                         (for-private-mentions :accept)
                                         (for-limited-accounts :accept))
  (assert (member for-not-following    '(:accept :filter :drop)))
  (assert (member for-not-followers    '(:accept :filter :drop)))
  (assert (member for-new-accounts     '(:accept :filter :drop)))
  (assert (member for-private-mentions '(:accept :filter :drop)))
  (assert (member for-limited-accounts '(:accept :filter :drop)))
  (decode-notification-policy(submit client
                                     (format NIL "/api/v2/notifications/policy")
                                     :http-method :patch
                                     :for-not-following    for-not-following
                                     :for-not-followers    for-not-followers
                                     :for-new-accounts     for-new-accounts
                                     :for-private-mentions for-private-mentions
                                     :for-limited-accounts for-limited-accounts)))

(defmethod find-results ((client v2:client) query &key account-id max-id min-id kind (exclude-unreviewed NIL e-p) (resolve NIL r-p) (limit 20) (offset 0) (following NIL f-p))
  (check-type account-id (or null string))
  (check-type max-id (or null string))
  (check-type min-id (or null string))
  (check-type kind (or null string))
  (assert (member kind '("accounts" "hashtags" "statuses") :test #'string=))
  (decode-results (query client "/api/v2/search"
                         :q query
                         :account-id account-id
                         :max-id max-id
                         :min-id min-id
                         :type   kind
                         :exclude-unreviewed (coerce-boolean exclude-unreviewed e-p)
                         :resolve (coerce-boolean resolve r-p)
                         :limit limit
                         :offset offset
                         :following (coerce-boolean following f-p))))
