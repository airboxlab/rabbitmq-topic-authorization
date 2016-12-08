# RabbitMQ Topic Authorization #

This plugin checks authorizations on routing keys. It can be used with internally-defined ACLs, as well as external authorization plugins (e.g. rabbitmq-auth-backend-http).

## Supported RabbitMQ Versions ##

This plugin targets RabbitMQ 3.6.0 and later versions.

## Installing ##

### Downloading ###

Check [https://github.com/airboxlab/rabbitmq-topic-authorization/releases](https://github.com/airboxlab/rabbitmq-topic-authorization/releases) page

### Building from source ###

To use this plugin you will have to build RabbitMQ from master and
then build the plugin against it, since it depends on a new
_interceptors_ API that's not yet available on the 3.5.x series.

Follow plugin development instructions:
[http://www.rabbitmq.com/plugin-development.html](http://www.rabbitmq.com/plugin-development.html).

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

