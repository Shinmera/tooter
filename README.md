## About Tooter
This is a client library implementing v1/v2 REST API protocol for [Mastodon](https://joinmastodon.org/).

## How To
Before you start, you'll need an account on some Mastodon instance. For the purposes of this documentation we'll simply pick [mastodon.social](https://mastodon.social). Once you've got Tooter loaded you'll first need to create a `client` instance.

    (defvar *client* (make-instance 'tooter:client
                       :base "https://mastodon.social"
                       :name "My Tooter Test App"))

Once you have a client set up, you need to authorise it against your account so that you can make requests on your behalf.

    (tooter:authorize *client*)

This will return an URL as its secondary value. Visit this URL in your browser, click on the authorize button, and copy the code it displays. Then call `authorize` again using this code.

    (tooter:authorize *client* "...")

If everything goes well this should return a fully authorised client instance.

    (tooter:account *client*)

If you are connecting to an instance that is running a version of the software that does not supports API version 2, is recommended that you switch client class to `tooter:client-v1`

    (setf *client* (change-class *client* 'tooter:client-v1))

From here on out you can make use of the full API Mastodon API. See the definition index below.

    (tooter:make-status *client* "Tooter works, woah nelly!")

## Persisting Client Settings
Once you have authorised your client, you will probably want to save the information somewhere so that the user doesn't need to re-authorise every time. To do this, simply save the `key`, `secret`, and `access-token` values from the client instance. If those are set, the `authorize` steps are not needed.

## Writing an Application Library
If you write an application on top of Mastodon and would like to offer this as another re-usable library, you should create a subclass of `client` that automatically provides the proper initargs for the `:name` and so forth. This ensures that your application is known under the correct name, but still lets the user specify the correct `:base`.

## Official Documentation

The official documentation for the REST API of mastodon can be found here:

https://docs.joinmastodon.org/
