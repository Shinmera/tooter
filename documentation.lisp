#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter)

;; client.lisp
(docs:define-docs
  (type request-failed
    "Error signalled when a request against the Mastodon API failed.

See CODE
See DATA
See MESSAGE")

  (function code
    "Returns the HTTP return code for the failed request.

See REQUEST-FAILED")

  (function data
    "Returns the data payload for the failed request as parsed JSON.

See REQUEST-FAILED")

  (function message
    "Returns the error message string for the failed request.

See REQUEST-FAILED")

  (function request
    "Perform a request against an API.

This is a wrapper around the underlying HTTP client, performing
automated error handling and data payload parsing.

In case the request returns with an HTTP return code other than 200,
an error of type REQUEST-FAILED is signalled.

See REQUEST-FAILED")

  (type client
    "Representation of an application client connecting to a Mastodon instance.

If you are creating a library providing some kind of service based on
Mastodon, you should create a wrapper function that fills in the fields
like NAME, REDIRECT, SCOPES, and WEBSITE, but lets the user specify the
BASE so that it can be used against any Mastodon instance.

The fields KEY, SECRET, and ACCESS-TOKEN are specific to your
application, Mastodon instance, and the user using the application. You
should save these fields away somewhere after they've been initialised
so that they can be re-used at a later point without having to re-
register and re-authorise.

The KEY and SECRET will be filled in by REGISTER. The ACCESS-TOKEN will
be filled in by AUTHORIZE once the user calls it with a valid
AUTHORIZATION-CODE retrieved by authorizing the application.

Generally the procedure for using a client is as follows: fill in the
BASE and NAME, call AUTHORIZE and instruct the user to follow the URL
from the second return value. Make the user call AUTHORIZE again with
the code from the website as the second argument. Once that's done,
you should be fine to make any calls necessary from there on out. Make
sure to persist the KEY, SECRET, and ACCESS-TOKEN for future use.
ACCESS-TOKENs should not expire unless the user revokes access manually.

CLIENT instances can be dumped to FASLs.

See BASE
See KEY
See SECRET
See ACCESS-TOKEN
See NAME
See REDIRECT
See SCOPES
See WEBSITE
See ACCOUNT
See REGISTER
See AUTHORIZE
See DEFAULT-HEADERS
See QUERY
See SUBMIT")

  (function base
    "Accessor to the base URL of the Mastodon instance the client connects to.

Typically this will be something like:

  https://mastodon.social
  https://mastodon.tymoon.eu
  etc.

See CLIENT")

  (function key
    "Accessor to the application key the client is using to connect.

Unless manually filled in, this will be automatically set by REGISTER.

See CLIENT
See REGISTER")

  (function secret
    "Accessor to the application secret the client is using to connect.

Unless manually filled in, this will be automatically set by REGISTER.

See CLIENT
See REGISTER")

  (function access-token
    "Accessor to the access token the client is using to authenticate requestts.

Unless manually filled in, this will be automatically set by AUTHORIZE.

See CLIENT
See AUTHORIZE")

  (function redirect
    "Accessor to the redirect URL used to complete the authentication flow.

Unless manually set, this is defaulted to \"urn:ietf:wg:oauth:2.0:oob\"
which will display the authentication code to the user on the website
during authorisation.

See CLIENT
See AUTHORIZE")

  (function scopes
    "Accessor to the list of scopes the client will have access to.

A scope can be one of :READ, :WRITE, :FOLLOW. Unless manually set, this
defaults to a list of all three scopes.

See CLIENT
See REGISTER")

  (function account
    "Accessor to the account the client is authorised as.

When reading, it will automatically try to fetch the account if it is
not yet known. Any operation (verify-credentials, update-credentials)
that returns your own user account will update this field automatically.

See CLIENT")

  (function default-headers
    "Returns additional headers to be sent with requests from the client.

See CLIENT")

  (function query
    "Performs a query request against the client.

The endpoint should be the full path on the server. The parameters
should be a plist of keys and values to be sent as request parameters.
The list is transformed via PARAM-PLIST->ALIST. By default this uses
the GET request method. You can specify a different method with the
special parameter :HTTP-METHOD.

Note that no matter what, the content-type of the request will be
application/x-www-form-urlencoded meaning it is not suitable to upload
files using QUERY.

See CLIENT
See PARAM-PLIST->ALIST
See SUBMIT")

  (function submit
    "Performs a submission request against the client.

The endpoint should be the full path on the server. The parameters
should be a plist of keys and values to be sent as request parameters.
The list is transformed via PARAM-PLIST->ALIST. By default this uses
the POST request method. You can specify a different method with the
special parameter :HTTP-METHOD.

Note that no matter what, the content-type of the request will be
multipart/form-data meaning it is not suitable to use for GET endpoints.

See CLIENT
See PARAM-PLIST->ALIST
See QUERY")

  (function register
    "Register the application on the Mastodon instance.

This will cause the KEY and SECRET to be set in the client if the
registration succeeds.

Returns the client instance, the key, and the secret.

See CLIENT")

  (function authorize
    "Authorise the client against the Mastodon instance.

This is a two-step process that requires user-interaction. First, call
this without an authorisation code. This will return NIL, and an URL
as the secondary value. Instruct the user to visit this URL and to
authorise the application. If you have set up a redirect, the user will
automatically visit the redirect page. Otherwise, the user will be
displayed a code. Instruct them to copy the code so that it can be
passed to AUTHORIZE somehow. Once AUTHORIZE is called again with an
authorisation code, it will attempt to obtain an access token. If
successful, it will return the client and the access token. Note that
by default the authorisation code is only valid for ten minutes and
obtaining an access token after it has expired will result in failure.

A successful completion of the authorisation process will automatically
set the access token in the client instance.

If the client does not have the KEY and SECRET set, then this function
will automatically call out to REGISTER to obtain them.

See CLIENT
See REGISTER"))

;; objects.lisp
(docs:define-docs
  (type entity
    "Superclass for all objects returned by Mastodon API queries.

See DECODE-ENTITY")

  (function decode-entity
    "Parses a data payload as the given entity type.

If the type is a symbol, a new instance of the given type is allocated
and then filled in via DECODE-ENTITY. If the data is a list, a new
instance is created and decoded for each entry in the list.")

  (type field
    "Represents a profile element as key value pair.

See NAME
See VALUE
See VERIFIED-AT
")

  (function name
    "Key value of the field.
See FIELD")

  (function value
    "Value of the field.

See FIELD")

  (function verified-at
    "Timesatmp when the server verified the url in the value of this field

See FIELD")

  (type account
    "Represents a user account on the mastodon instance.

Note that this may also be a remote account that resides on another
instance.

See ID
See USERNAME
See ACCOUNT-NAME
See DISPLAY-NAME
See LOCKED
See CREATED-AT
See FOLLOWERS-COUNT
See FOLLOWING-COUNT
See STATUSES-COUNT
See NOTE
See URL
See AVATAR
See AVATAR-STATIC
See HEADER
See HEADER-STATIC
See MOVED
See FIELDS
See BOT
See SOURCE")

  (function id
    "Returns the ID of the object as a string.

This ID is used in the API to query the object itself, or properties
related to it. The ID may be an integer, or some other structure encoded
as a string, depending on the instance the object stems from.

About the only thing you can portably do with an ID is determine identity
via STRING=.

See ACCOUNT
See ATTACHMENT
See USER-LIST
See MENTION
See NOTIFICATION
See PUSH-SUBSCRIPTION
See RELATIONSHIP
See REPORT
See STATUS")

  (function username
    "Returns the username of the object.

The username does not include the instance/domain name.

See ACCOUNT
See MENTION")

  (function account-name
    "Returns representative name of the account.

This includes the instance's suffix if the account resides on a remote
instance. Otherwise this is equal to the username.

See ACCOUNT
See MENTION")

  (function display-name
    "Returns the display name of the account.

See ACCOUNT")

  (function locked
    "Returns whether the account is locked and requires confirming follow requests.

See ACCOUNT")

  (function created-at
    "Returns the universal-time timestamp on which the given object was created.

See ACCOUNT
See NOTIFICATION
See STATUS")

  (function followers-count
    "Returns the number of followers the account has.

See ACCOUNT")

  (function following-count
    "Returns the number of users this account is following.

See ACCOUNT")

  (function statuses-count
    "Returns the number of statuses the account has made.

See ACCOUNT")

  (function note
    "Returns the account's note or description.

See ACCOUNT")

  (function url
    "Returns the full URL to the object on its home instance.

See ACCOUNT
See ATTACHMENT
See CARD
See EMOJI
See INSTANCE
See MENTION
See STATUS
See TAG")

  (function avatar
    "Returns the full URL to the account's avatar image.

This may be an animated image.

See ACCOUNT")

  (function avatar-static
    "Returns the full URL to the account's static avatar image.

See ACCOUNT")

  (function header
    "Returns the full URL to the account's header image.

This may be an animated image.

See ACCOUNT")

  (function header-static
    "Returns the full URL to the account's static header image.

See ACCOUNT")

  (function moved
    "If not NIL, returns the name of the account to which this account has moved.

See ACCOUNT")

  (function fields
    "Returns an alist of account profile fields.

This list is limited to a maximum of four fields.
The fields have custom keys and values that the user can specify.

See ACCOUNT")

  (function bot
    "Returns whether the account is a bot.

See ACCOUNT")

  (function source
    "Returns a hash table of extra information about the account.

This is only set for accounts retrieved through verify-credentials.

See ACCOUNT")

  (type activity
    "Representation of statistsics about an instance activity.

See WEEK
See STATUSES
See LOGINS
See REGISTRATION")


  (function week
    "Timestamp for the first day of this week stats.

See ACTIVITY")

  (function statuses
   "Number of statuses for this week

See ACTIVITY")

  (function logins
   "Number of users login for this week

See ACTIVITY")

  (function registrations
    "Number of registration for this week

See ACTIVITY")

  (type application
    "Representation of an application as registered on a Mastodon instance.

See NAME
See WEBSITE")

  (function name
    "Returns the name of the object.

See CLIENT
See APPLICATION
See TAG")

  (function website
    "Returns the website URL of the object.

See APPLICATION
See CLIENT")

  (type attachment
    "Representation of a media attachment for a status.

See ID
See KIND
See URL
See REMOTE-URL
See PREVIEW-URL
See TEXT-URL
See METADATA
See DESCRIPTION")

  (function kind
    "Returns the type/kind of object this instance is representing.

See ATTACHMENT
See CARD
See NOTIFICATION")

  (function remote-url
    "Returns the remote URL if the attachment resides on a remote instance.

See ATTACHMENT")

  (function preview-url
    "Returns the URL for the media's preview, which may be resized and cropped.

See ATTACHMENT")

  (function text-url
    "Returns a shorter URL for usage in text snippets if the attachment is on the local instance.

See ATTACHMENT")

  (function metadata
    "Returns a metadata instance for additional information about the attachment's media.

See ATTACHMENT")

  (function description
    "Returns a textual description of the contents of the object.

See ATTACHMENT
See CARD
See INSTANCE")

  (type metadata
    "This object holds metadata information for media objects.

See SMALL
See ORIGINAL
See FOCUS")

  (function small
    "Returns metadata information for the small version of the attachment media, if available.

See METADATA
See IMAGE-METADATA
See VIDEO-METADATA")

  (function original
    "Returns metadata information for the original version of the attachment media, if available.

See METADATA
See IMAGE-METADATA
See VIDEO-METADATA")

  (function focus
    "Returns a cons cell of the X and Y coordinates on which the focus in the media should lie.

This is useful for determining a good cropping region.

See METADATA")

  (type image-metadata
    "Metadata for static images.

See WIDTH
See HEIGHT
See SIZE
See ASPECT")

  (function width
    "Returns the width of the media or card.

See CARD
See IMAGE-METADATA
See VIDEO-METADATA")

  (function height
    "Returns the height of the media or card.

See CARD
See IMAGE-METADATA
See VIDEO-METADATA")

  (function size
    "Returns the size of the image.

See IMAGE-METADATA")

  (function aspect
    "Returns the aspect ratio of the image.

See IMAGE-METADATA")

  (type video-metadata
    "Metadata for animated videos.

See WIDTH
See HEIGHT
See FRAME-RATE
See DURATION
See BITRATE")

  (function frame-rate
    "Returns the frame-rate of the video in FPS.

See VIDEO-METADATA")

  (function duration
    "Returns the duration of the video in seconds.

See VIDEO-METADATA")

  (function bitrate
    "Returns the bitrate of the video in ???.

See VIDEO-METADATA")

  (type card
    "Cards represent all information to summarise a status.

See URL
See TITLE
See DESCRIPTION
See IMAGE
See KIND
See AUTHOR-NAME
See AUTHOR-URL
See PROVIDER-NAME
See PROVIDER-URL
See HTML
See WIDTH
See HEIGHT")

  (function title
    "Returns the title of the object.

See CARD
See INSTANCE
See USER-LIST")

  (function image
    "Returns the URL of the card's image, if any.

See CARD")

  (function author-name
    "Returns the name of the status' author.

See CARD")

  (function author-url
    "Returns the URL to the status' author's profile.

See CARD")

  (function provider-name
    "Returns the name of the status' instance.

See CARD")

  (function provider-url
    "Returns the URL to the status' instance.

See CARD")

  (function html
    "Returns an HTML snippet for the card's OEmbed data.

See CARD")

  (type context
    "An object representing a status' context.

Contexts are used to represent reply chains.

See ANCESTORS
See DESCENDANTS")

  (function ancestors
    "Returns the list of ancestor status instances in the context chain.

See CONTEXT")

  (function descendants
    "Returns the list of descendant status instances in the context chain.

See CONTEXT")

  (type emoji
    "Representation of a custom emoticon on the instance.

See SHORTCODE
See STATIC-URL
See URL")

  (function shortcode
    "Returns the short code to display the emoji.

See EMOJI")

  (function static-url
    "Returns the static image of the emoji.

See EMOJI")

  (type instance
    "Representation of a Mastodon server instance.

See URI
See TITLE
See DESCRIPTION
See EMAIL
See VERSION
See URLS
See LANGUAGES
See CONTACT-ACCOUNT")

  (function uri
    "Returns the instance's URI.

See INSTANCE
See STATUS")

  (function email
    "Returns the primary contact email address for the instance.

See INSTANCE")

  (function version
    "Returns the instance's Mastodon version.

See INSTANCE")

  (function urls
    "Returns a list of URLs for the streaming API.

See INSTANCE")

  (function languages
    "A list of ISO-6391 language codes that the instance advertises.

See INSTANCE")

  (function contact-account
    "Returns the account instance that represents the contact person for this instance.

See INSTANCE")

  (type user-list
    "Represents a list of users.

See ID
See TITLE")

  (type mention
    "Representation of an account mention in a status.

See URL
See USERNAME
See ACCOUNT-NAME
See ID")

  (type notification
    "Representation of a new status update notification.

See ID
See KIND
See CREATED-AT
See ACCOUNT
See STATUS")

  (function status
    "Returns the status the notification is about.

See NOTIFICATION")

  (type push-subscription
    "Representation of a push notification subscription.

See ID
See ENDPOINT
See SERVER-KEY
See ALERTS")

  (function endpoint
    "The endpoint URL to which push notifications are delivered.

See PUSH-SUBSCRIPTION")

  (function server-key
    "The public key signature for verification of deliveries.

See PUSH-SUBSCRIPTION")

  (function alerts
    "What kinds of alerts push notifications are being sent out for... I think.

See PUSH-SUBSCRIPTION")

  (type relationship
    "Representation of a relationship between the current user and another account.

See ID
See FOLLOWING
See FOLLOWED-BY
See BLOCKING
See MUTING
See MUTING-NOTIFICATIONS
See REQUESTED
See DOMAIN-BLOCKING")

  (function following
    "Returns whether you are following this account.

See RELATIONSHIP")

  (function followed-by
    "Returns whether the account is following you.

See RELATIONSHIP")

  (function blocking
    "Returns whether the account is blocked by you.

See RELATIONSHIP")

  (function muting
    "Returns whether the account is muted by you.

See RELATIONSHIP")

  (function muting-notifications
    "Returns whether notifications from the account are muted by you.

See RELATIONSHIP")

  (function requested
    "Returns whether you have requested a follow to the account.

See RELATIONSHIP")

  (function domain-blocking
    "Returns whether you are blocking the account's domain.

See RELATIONSHIP")

  (type report
    "Representation of an incident report.

See ID
See ACTION-TAKEN")

  (function action-taken
    "Returns what kind of action was taken in response to the report.

See REPORT")

  (type results
    "Representation of a search result.

See ACCOUNTS
See STATUSES
See HASHTAGS")

  (function accounts
    "Returns a list of matching accounts.

See RESULTS")

  (function statuses
    "Returns a list of matching statuses.

See RESULTS")

  (function hashtags
    "Returns a list of matching hashtags as strings.

See RESULTS")

  (type status
    "Representation of a status update.

\"Statuses\", \"toots\", or \"tweets\" are the primary content of a
Mastodon instance.

See ID
See URI
See URL
See ACCOUNT
See IN-REPLY-TO-ID
See IN-REPLY-TO-ACCOUNT-ID
See PARENT
See CONTENT
See CREATED-AT
See EMOJIS
See DISCOVERABLE
See REBLOGS-COUNT
See FAVOURITES-COUNT
See REBLOGGED
See FAVOURITED
See MUTED
See SENSITIVE
See SPOILER-TEXT
See VISIBILITY
See MEDIA-ATTACHMENTS
See MENTIONS
See TAGS
See APPLICATION
See LANGUAGE
See BOT
See PINNED")

  (function in-reply-to-id
    "Returns the ID of the status this status is a reply to.

See STATUS")

  (function in-reply-to-account-id
    "Returns the ID of the account this status is a reply to.

See STATUS")

  (function parent
    "Returns the original status this status is a reblog of.

See STATUS")

  (function content
    "Returns the status' content as an HTML string.

See STATUS")

  (function emojis
    "Returns an array of emoji used in the status.

See STATUS
See EMOJI")

  (function discoverable
    "Return non nil if the status could be displayed in a public directory of the instance.

See STATUS")

  (function reblogs-count
    "Returns the number of reblogs this status has received.

See STATUS")

  (function favourites-count
    "Returns the number of favourites this status has received.

See STATUS")

  (function reblogged
    "Returns whether you have reblogged this status.

See STATUS")

  (function favourited
    "Returns whether you have favourited this status.

See STATUS")

  (function muted
    "Returns whether this status should be muted.

See STATUS")

  (function sensitive
    "Returns whether this status contains sensitive media attachments.

See STATUS")

  (function spoiler-text
    "Returns the spoiler text for the status.

See STATUS")

  (function visibility
    "Returns the default visibility of the status.

Can be one of :PUBLIC :UNLISTED :PRIVATE :DIRECT

See STATUS")

  (function media-attachments
    "Returns a list of up to four media attachments.

See ATTACHMENT
See STATUS")

  (function mentions
    "Returns an array of mentions used in the status.

See STATUS
See MENTION")

  (function tags
    "Returns an array of tags used in the status.

See STATUS
See TAG")

  (function application
    "Returns the application with which this status was made.

See STATUS
See APPLICATION")

  (function language
    "Returns the ISO-6391 code for the language this status is in.

See STATUS")

  (function bot
    "Returns non nil if thids account is a bot.

See STATUS")

  (function pinned
    "Returns whether this status is pinned on the user's profile.

See STATUS")

  (type tag
    "Representation of a hashtag used in a status.

See NAME
See URL
See HISTORY")

  (function history
    "Returns an array describing daily usage information of the hashtag.

See TAG
See TAG-HISTORY")

  (type tag-history
    "Representation of the usage history of a hashtag.

See DAY
See USE-COUNT
See ACCOUNT-COUNT")

  (function day
    "Returns the universal-time of the day for which this usage history is.

See TAG-HISTORY")

  (function use-count
    "Returns the number of statuses that made use of the hashtag on that day.

See TAG-HISTORY")

  (function account-count
    "Returns the number of accounts that made use of the hashtag on that day.

See TAG-HISTORY")

  (type conversation
    "Representation of a private conversation (currently a conversation is private if visibility of a status is :DIRECT)

See ID
See ACCOUNTS
See UNREAD
See LAST-STATUS
See VISIBILITY
See STATUS")

  (function unread
    "Returns non nil if this converation has been marked not red")

  (function last-status
    "Returns the last STATUS of the conversation"))

;; queries.lisp
(docs:define-docs
  (function find-account
    "Find an account with the specified ID.

This only works for accounts that are local to the instance.

See CLIENT
See ACCOUNT")

  (function verify-credentials
    "Returns your own account.

This updates the ACCOUNT field in the client.

See CLIENT
See ACCOUNT")

  (function update-credentials
    "Update some of the user's profile settings.

FIELDS should be a plist of the desired fields, with alternating field
names and field values. The keys can be strings to more easily control
the look of the fields.

Returns the updated account.
This updates the ACCOUNT field in the client.

See CLIENT
See ACCOUNT")

  (function get-followers
    "Return a list of followers for the account.

The account can be an ACCOUNT instance, an account ID, or T for
yourself.

See CLIENT
See ACCOUNT")

  (function get-following
    "Return a list of accounts the account is following.

The account can be an ACCOUNT instance, an account ID, or T for
yourself.

See CLIENT
See ACCOUNT")

  (function get-statuses
    "Return a list of statuses for the account.

The account can be an ACCOUNT instance, an account ID, or T for
yourself.

See CLIENT
See ACCOUNT
See STATUS")

  (function follow
    "Follow a new account.

The account can be either an account ID, or an ACCOUNT instance.
It can also be a URI string for a remote account.

Returns the resulting relationship if the account was local, and the
local representation of the remote account if it was remote.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function unfollow
    "Unfollow an account.

The account can be either an account ID, or an ACCOUNT instance.

Returns the resulting relationship.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function block
    "Block a new account or domain.

The account can be either an account ID, or an ACCOUNT instance.
It can also be a string, in which case the corresponding domain is
blocked.

Returns the resulting relationship in the case of an account, or T
in the case of a domain.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function unblock
    "Unblock an account or domain.

The account can be either an account ID, or an ACCOUNT instance.
It can also be a string, in which case the corresponding domain is
unblocked.

Returns the resulting relationship in the case of an account, or T
in the case of a domain.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function mute
    "Mute a new account or conversation.

The account can be either an account ID, or an ACCOUNT instance.
It can also be a STATUS in which case that conversation is muted.

Returns the resulting relationship for an account mute, or T for a
conversation mute.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function unmute
    "Unmute a new account or conversation.

The account can be either an account ID, or an ACCOUNT instance.
It can also be a STATUS in which case that conversation is unmuted.

Returns the resulting relationship for an account unmute, or T for a
conversation unmute.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function get-activity
    "Get statistics from an instance

See CLIENT
See ACTIVITY")

  (function relationships
    "Returns a list of relationships for the given accounts.

The accounts can be a list of IDs or ACCOUNT instances.

See CLIENT
See ACCOUNT
See RELATIONSHIP")

  (function search-accounts
    "Search for accounts on the Mastodon network.

This is the only way of retrieving accounts outside of the local
instance.

Returns a list of matching accounts.

See CLIENT
See ACCOUNT")

  (function blocks
    "Retrieve a list of blocked accounts.

See CLIENT
See ACCOUNT")

  (function blocked-domains
    "Retrieve a list of blocked domains.

See CLIENT")

  (function favourites
    "Retrieve a list of favourited statuses.

See CLIENT
See STATUS")

  (function follow-requests
    "Retrieve a list of accounts that requested to follow you.

See ACCOUNT
See CLIENT")

  (function accept-request
    "Accept the follow request of the given account.

See ACCOUNT
See CLIENT")

  (function reject-request
    "Reject the follow request of the given account.

See CLIENT
See ACCOUNT")

  (function follow-remote
    "Follow a remote account.

Typically you do not need to use this function, as FOLLOW automatically
performs the correct action.

Returns the local representation of the followed account.

See FOLLOW
See CLIENT
See ACCOUNT")

  (function instance
    "Retrieve the instance information that the client is connected to.

See INSTANCE
See CLIENT")

  (function emojis
    "Retrieve a list of custom emojis present on the instance.

See CLIENT
See EOMJI")

  (function user-lists
    "Retrieve a list of up to twenty of the account's user lists.

The account can be an ACCOUNT instance, an account ID, or T for
yourself.

See USER-LIST
See CLIENT")

  (function user-list-accounts
    "Retrieve a list of accounts in the user list.

The user list can be either a USER-LIST instance, or an ID of one.
This is SETFable for convenience, though it will usually be much more
efficient to simply use ADD/REMOVE-USER-LIST-ACCOUNTS instead.

See USER-LIST
See CLIENT")

  (function find-list
    "Retrieve the user list with the given ID.

See USER-LIST
See CLIENT")

  (function make-user-list
    "Create a new user list with the given title.

See USER-LIST
See CLIENT")

  (function update-user-list
    "Update the user list's properties.

The list can be either a USER-LIST instance, or an ID of one.

This is not used to add or remove accounts from the list. See
ADD/REMOVE-USER-LIST-ACCOUNTS for that.

See USER-LIST
See CLIENT")

  (function delete-user-list
    "Delete an existing user list.

The list can be either a USER-LIST instance, or an ID of one.

Returns T.

See USER-LIST
See CLIENT")

  (function add-user-list-accounts
    "Add new accounts to the user-list.

The list can be either a USER-LIST instance, or an ID of one.
The accounts in the list can be either account instances, or IDs of
accounts.

See ACCOUNT
See CLIENT
See USER-LIST")

  (function remove-user-list-accounts
    "Remove existing accounts from the user-list.

The list can be either a USER-LIST instance, or an ID of one.
The accounts in the list can be either account instances, or IDs of
accounts.

See ACCOUNT
See CLIENT
See USER-LIST")

  (function make-media
    "Create a new media attachment.

FILE must be a pathname. FOCUS should be, if given, a cons of X and Y
coordinates on which the focus should be put in the media.

Returns the new media ATTACHMENT instance.

See ATTACHMENT
See CLIENT")

  (function update-media
    "Updates the media attachment's metadata.

This can only be performed before the attachment is used in a status.

Returns the new media ATTACHMENT instance.

See CLIENT
See ATTACHMENT")

  (function mutes
    "Returns a list of accounts that you have muted.

See ACCOUNT
See CLIENT")

  (function notifications
    "Returns a list of notifications about status updates.

See NOTIFICATION
See CLIENT")

  (function find-notification
    "Retrieve the notification of the given ID.

See NOTIFICATION
See CLIENT")

  (function delete-notification
    "Delete or dismiss the notification.

The notification can either be a NOTIFICATION instance, an ID of one,
or T for all notifications.

Returns T.

See NOTIFICATION
See CLIENT")

  (function make-subscription
    "Create or update a push notification subscription.

ALERTS should be a list of desired alerts:
  :FOLLOWS :FAVOURITES :REBLOGS :MENTIONS

Returns the resulting PUSH-SUBSCRIPTION instance.

See CLIENT
See PUSH-SUBSCRIPTION")

  (function subscription
    "Retrieve the current push subscription settings.

See CLIENT
See PUSH-SUBSCRIPTION")

  (function delete-subscription
    "Delete the existing push subscription.

Returns T.

See CLIENT")

  (function reports
    "Returns a list of submitted reports.

See REPORT
See CLIENT")

  (function make-report
    "Files a new report against the given account.

The account can be an ACCOUNT instance, or an ID of one.

STATUSES must be a list of either STATUS instances or IDs of such that
refer to the offending statuses. COMMENT must be a string describing
the offence.

See ACCOUNT
See STATUS
See CLIENT")

  (function find-results
    "Search the Mastodon instance for matching tags, accounts, or statuses.

See RESULTS
See CLIENT")

  (function find-status
    "Retrieve the status with the given ID.

See CLIENT
See STATUS")

  (function context
    "Retrieve the context of a status.

The status can either be a STATUS instance, or an ID of one.

See CLIENT
See STATUS
See CONTEXT")

  (function card
    "Retrieve the card of a status.

The status can either be a STATUS instance, or an ID of one.

See STATUS
See CLIENT
See CARD")

  (function rebloggers
    "Retrieve the list of accounts that reblogged the status.

The status can either be a STATUS instance, or an ID of one.

See STATUS
See CLIENT
See ACCOUNT")

  (function favouriters
    "Retrieve the list of accounts that favourited the status.

The status can either be a STATUS instance, or an ID of one.

See STATUS
See CLIENT
See ACCOUNT")

  (function make-status
    "Create a new status.

The arguments should be seen as follows:
  STATUS       --- The content of the status update. Should be plain
                   text.
  IN-REPLY-TO  --- May be a STATUS instance or ID to which this status
                   should reply to.
  MEDIA        --- May be an attachment, or a list of up to four
                   attachments. See below for the handling of media
                   attachments.
  SENSITIVE    --- Whether the content contains sensitive material.
  SPOILER-TEXT --- Denotes the text to show in place of the content
                   before the content is revealed. Forces sensitive.
  VISIBILITY   --- May denote how visible the status should be. Can be
                   one of the following:
                    :DIRECT    The status can only be seen by mentions
                    :PRIVATE   The status can only be seen by you
                    :UNLISTED  The status can only be seen by link
                    :PUBLIC    The status appears publicly on timelines
  LANGUAGE     --- May be an ISO-639 code of the language the status
                   text is in.

Media attachments can be one of the following types:
  INTEGER      --- The referenced attachment is used.
  ATTACHMENT   --- The referenced attachment is used.
  PATHNAME     --- A new media attachment is created automatically and
                   its new ID is used.

Returns the newly created status instance.

See CLIENT
See STATUS
See ATTACHMENT")

  (function delete-status
    "Delete the given status.

The status can either be a STATUS instance, or an ID of one.

Returns T.

See STATUS
See CLIENT")

  (function reblog
    "Reblog the given status.

The status can either be a STATUS instance, or an ID of one.

Returns the new status which is a reblog of the given status.

See STATUS
See CLIENT")

  (function unreblog
    "Remove the reblog of the given status.

The status can either be a STATUS instance, or an ID of one.

Returns the original status that was given as a fresh instance.

See STATUS
See CLIENT")

  (function favourite
    "Favourite the given status.

The status can either be a STATUS instance, or an ID of one.

Returns the referenced status.

See STATUS
See CLIENT")

  (function unfavourite
    "Unfavourite the given status.

The status can either be a STATUS instance, or an ID of one.

Returns the referenced status.

See STATUS
See CLIENT")

  (function pin
    "Pin the given status to your profile.

The status can either be a STATUS instance, or an ID of one.

Returns the referenced status.

See STATUS
See CLIENT")

  (function unpin
    "Unpin the given status from your profile.

The status can either be a STATUS instance, or an ID of one.

Returns the referenced status.

See STATUS
See CLIENT")

  (function mute-conversation
    "Mute the conversation of the given status.

This means you will no longer receive mention notifications for the
given status' conversation thread.

Returns the referenced status.

See STATUS
See CLIENT")

  (function unmute-conversation
    "Unmute the conversation of the given status.

This means you will receive mention notifications for the given status'
conversation thread again.

Returns the referenced status.

See STATUS
See CLIENT")

  (function timeline
    "Return statuses for the specified timeline.

The KIND can be one of the following:
  :HOME      --- Return statuses for your home timeline. This includes
                 your own and statuses of all accounts you follow.
  :PUBLIC    --- Return statuses for your instance's public timeline.
  STRING     --- Return statuses for the given hashtag.
  INTEGER    --- Return statuses for the given user-list's accounts.
  USER-LIST  --- Return statuses for the given user-list's accounts.

See STATUS
See CLIENT
See USER-LIST")

  (function trends
    "Return a list of trending hashtags.

See CLIENT")

  (function conversations
    "Return a list of conversation.

See CONVERSATION
See CLIENT"))

;; toolkit.lisp
(docs:define-docs
  (function plain-format-html
    "Attempts to remove all HTML tags and translate the entities into standard characters.

The exception is <br /> tags, which are turned into Linefeeds.
This function is useful for plaintext formatting of status content."))
