#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:tooter-objects
  (:nicknames #:org.shirakumo.tooter.objects)
  (:use)
  ;; objects.lisp
  (:export
   #:entity
   #:decode-entity
   #:account
   #:id
   #:username
   #:account-name
   #:display-name
   #:locked
   #:discoverable
   #:created-at
   #:followers-count
   #:following-count
   #:statuses-count
   #:note
   #:url
   #:avatar
   #:avatar-static
   #:header
   #:header-static
   #:moved
   #:fields
   #:bot
   #:source
   #:application
   #:name
   #:website
   #:attachment
   #:id
   #:kind
   #:url
   #:remote-url
   #:preview-url
   #:text-url
   #:metadata
   #:description
   #:metadata
   #:small
   #:original
   #:focus
   #:image-metadata
   #:width
   #:height
   #:size
   #:aspect
   #:video-metadata
   #:width
   #:height
   #:frame-rate
   #:duration
   #:bitrate
   #:card
   #:url
   #:title
   #:description
   #:image
   #:kind
   #:author-name
   #:author-url
   #:provider-name
   #:provider-url
   #:html
   #:width
   #:height
   #:context
   #:ancestors
   #:descendants
   #:emoji
   #:shortcode
   #:static-url
   #:url
   #:instance
   #:uri
   #:title
   #:description
   #:email
   #:version
   #:urls
   #:languages
   #:contact-account
   #:user-list
   #:id
   #:title
   #:mention
   #:url
   #:username
   #:account
   #:id
   #:notification
   #:id
   #:kind
   #:created-at
   #:account
   #:status
   #:push-subscription
   #:id
   #:endpoint
   #:server-key
   #:alerts
   #:relationship
   #:id
   #:following
   #:followed-by
   #:blocking
   #:muting
   #:muting-notifications
   #:requested
   #:domain-blocking
   #:report
   #:id
   #:action-taken
   #:results
   #:accounts
   #:statuses
   #:hashtags
   #:status
   #:id
   #:uri
   #:url
   #:account
   #:in-reply-to-id
   #:in-reply-to-account-id
   #:parent
   #:content
   #:created-at
   #:emojis
   #:reblogs-count
   #:favourites-count
   #:reblogged
   #:favourited
   #:muted
   #:sensitive
   #:spoiler-text
   #:visibility
   #:media-attachments
   #:mentions
   #:tags
   #:application
   #:language
   #:pinned
   #:tag
   #:name
   #:url
   #:history
   #:tag-history
   #:day
   #:use-count
   #:account-count))

(defpackage #:tooter-queries
  (:nicknames #:org.shirakumo.tooter.queries)
  (:use #:org.shirakumo.tooter.objects)
  ;; queries.lisp
  (:export
   #:find-account
   #:verify-credentials
   #:update-credentials
   #:get-followers
   #:get-following
   #:get-statuses
   #:follow
   #:unfollow
   #:block
   #:unblock
   #:mute
   #:unmute
   #:relationships
   #:search-accounts
   #:blocks
   #:blocked-domains
   #:favourites
   #:follow-requests
   #:accept-request
   #:reject-request
   #:follow-remote
   #:instance
   #:emojis
   #:user-lists
   #:user-list-accounts
   #:find-list
   #:make-user-list
   #:update-user-list
   #:delete-user-list
   #:add-user-list-accounts
   #:remove-user-list-accounts
   #:make-media
   #:update-media
   #:mutes
   #:notifications
   #:find-notification
   #:delete-notification
   #:make-subscription
   #:subscription
   #:delete-subscription
   #:reports
   #:make-report
   #:find-results
   #:find-status
   #:context
   #:card
   #:rebloggers
   #:favouriters
   #:make-status
   #:delete-status
   #:reblog
   #:unreblog
   #:favourite
   #:unfavourite
   #:pin
   #:unpin
   #:mute-conversation
   #:unmute-conversation
   #:timeline-tag
   #:timeline
   #:trends))

(defpackage #:tooter-client
  (:nicknames #:org.shirakumo.tooter.client)
  (:use #:org.shirakumo.tooter.objects)
  ;; client.lisp
  (:export
   #:request-failed
   #:code
   #:data
   #:message
   #:request
   #:client
   #:base
   #:key
   #:secret
   #:access-token
   #:name
   #:redirect
   #:scopes
   #:website
   #:default-headers
   #:query
   #:submit
   #:register
   #:authorize)
  ;; toolkit.lisp
  (:export
   #:plain-format-html))

(defpackage #:tooter
  (:nicknames #:org.shirakumo.tooter)
  (:use #:cl #:tooter-objects #:tooter-queries #:tooter-client)
  (:shadowing-import-from #:tooter-queries #:block))

(dolist (package '(#:tooter-objects #:tooter-queries #:tooter-client))
  (do-symbols (symbol package)
    (export symbol '#:tooter)))
