# RabbitMQ Topic Authorization #

This plugin checks authorizations on routing keys. It can be used with internally-defined ACLs, as well as external authorization plugins (e.g. rabbitmq-auth-backend-http).

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

To use this plugin you will have to build RabbitMQ from master and
then build the plugin against it, since it depends on a new
_interceptors_ API that's not yet available on the 3.5.x series.

Install and setup the RabbitMQ Public Umbrella as explained here:
[http://www.rabbitmq.com/plugin-development.html#getting-started](http://www.rabbitmq.com/plugin-development.html#getting-started).

Then `cd` into the umbrella folder and type:

    $ git clone git://github.com/rabbitmq/rabbitmq-topic-authorization.git
    $ cd rabbitmq-topic-authorization
    $ make

Finally copy all the `*.ez` files inside the `dist` folder to the
`$RABBITMQ_HOME/plugins` folder. Don't copy the file
`rabbit_common-x.y.z` since it's not needed inside the broker
installation.

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_topic_authorization
```

The plugin will then hook into the `basic.publish`, `exchange.bind` and `queue.bind` processes in order to
check current user authorizations against related routing key.

## Limitations ##

The plugin hooks into the `basic.publish`, `exchange.bind` and `queue.bind` paths, so there could be a significant performance impact, especially when using external authentication mechanisms (e.G. ldap or http auth plugins). It uses a channel-scoped cache to reduce latency.

## LICENSE ##

See the LICENSE file

