# RabbitMQ Topic Authorization #

This plugin checks authorizations on routing keys. It can be used with internally-defined ACLs, as well as external authorization plugins (e.g. rabbitmq-auth-backend-http).

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

To use this plugin you will have to build RabbitMQ from master and
then build the plugin against it, since it depends on a new
_interceptors_ API that's not yet available on the 3.5.x series.

Follow plugin development instructions:
[http://www.rabbitmq.com/plugin-development.html](http://www.rabbitmq.com/plugin-development.html).

TL;DR: 

    $ git clone https://github.com/rabbitmq/rabbitmq-server.git
    $ cd rabbitmq-server
    $ make
    $ cd ../
    $ git clone git://github.com/airboxlab/rabbitmq-topic-authorization.git
    $ cd rabbitmq-topic-authorization
    $ make dist

Note: if you get a 

```bash
error: rabbitmq-components.mk must be updated!
```

Execute

    $ make rabbitmq-components-mk

Finally copy the `rabbitmq_topic_authorization.ez` file inside the `plugins` folder to the
`$RABBITMQ_HOME/plugins` folder. Don't copy the file
`rabbit_common-x.y.z` since it's not needed inside the broker
installation.

## Usage ##

Just enable the plugin with the following command:

```bash
rabbitmq-plugins enable rabbitmq_topic_authorization
```

The plugin will then hook into the `basic.publish`, `exchange.bind`, `exchange.unbind`, `queue.bind` and `queue.unbind` processes in order to
check current user authorizations against related routing key.

If you're using [rabbitmq-auth-backend-http plugin](https://github.com/rabbitmq/rabbitmq-auth-backend-http), you will get a call to the `resource/` end point with `?resource=routing_key&name=<routing key definition>`

## Limitations ##

The plugin hooks into the `basic.publish`, `exchange.bind` and `queue.bind` paths, so there could be a significant performance impact, especially when using external authentication mechanisms (e.G. ldap or http auth plugins). It uses a channel-scoped cache to reduce latency.

## LICENSE ##

See the LICENSE file

