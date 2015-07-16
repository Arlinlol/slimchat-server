
# SlimChat移动即时消息框架

Draft

## SlimChat简介

SlimChat是基于HTTP、MQTT协议的轻量移动即时消息框架。SlimChat主要用于手机、物联网设备、Web的消息推送或即时消息。


## SlimChat设计目标

1. 为手机、IoT设备、Web应用设计

2. HTTP与MQTT协议承载JSON报文

3. 可快速与手机、IoT设备、Web集成


## SlimChat架构设计


![Architecture](http://slimchat.io/img/SlimChat1.jpg)

应用服务器: 用户关系数据，应用业务数据，图片语音视频上传、处理与存储。

消息服务器: MQTT协议消息服务器，Pub/Sub接收发送即时消息，MQTT长连接维持。

数据服务器: Sql数据库存储用户关系或业务数据，Mongodb, Redis库存储离线消息数据。


## SlimChat消息路由

### 文本即时消息路由流程

![MsgRoute](http://slimchat.io/img/MsgRoute.png)

```
title Message Route Sequence

ClientA->Broker: Send Message
Broker-->ClientB: Push Message
ClientB-->Broker: Ack Message
Broker->ClientA: Ack Message
```

### 图片语音视频消息流程

![ImgRoute](http://slimchat.io/img/ImgRoute.png)

```
title BigImg/Audio/Video Route Sequence

ClientA->AppServer: Upload File
AppServer-->Broker: Forward Message(URL)
Broker-->ClientB: Push Message(URL)
ClientB->AppServer: HTTP GET(URL)
```


ClientA --> AppServer: 上传
AppServer --> ClientA: 成功失败?
ClientA --> Broker: 文件发送成功
Broker --> ClientB: 文件发送


## SlimChat路由寻址

消息路由的端点(Endpoint)可以是一个群组、用户或终端，每一个端点(Endpoint)由唯一的OID(Object ID)标识。

### OID

端点OID是层次体系的地址标识，即时消息通过OID进行路由寻址。


端点        |   OID 地址 
-----------|-----------------
通用        |  node/${nodeId}
群组        |  room/${roomId}
用户        |  user/${uid}
终端        |  user/${uid}/client/${clientId}

### 多终端同步

同一用户可从多终端登录系统，支持多终端消息同步，也可支持终端一对一。

例如用户1(uid1)从c1, c2两个终端登录，会分别注册路由地址:

user/uid1/client/c1

user/uid1/client/c2

用户2可以通过向地址'user/uid1'发布消息，同步到用户1的所有终端；

或向'user/uid1/client/c2'发布消息，只发布给用户1的c2终端。

### 跨域消息路由支持

SlimChat通过Bridge模式支持跨域消息路由，类似XMPP Federation模式。SlimChat消息服务器分配一个domain地址，每个端点通过前缀'domain/${domain}'转换为跨域地址。

用户端点的跨域地址:

domain/${domain}/user/${uid}

跨域端点间消息路由:

![Cross Domain](http://slimchat.io/img/CrossDomainRoute.png)

```
title Message(Cross Domain) Route Sequence

ClientA->BrokerA: Publish to 'domain/B/user/B'
BrokerA->BrokerB: Publish to 'user/B'
BrokerB-->ClientB: Push Message
ClientB->BrokerB: Reply to 'domain/A/user/A'
BrokerB->BrokerA: Publish to 'user/A'
BrokerA-->ClientA: Push Reply
```

## SlimChat消息报文

SlimChat通过HTTP、MQTT协议承载JSON格式的即时消息:

MQTT报文(Publish)| 即时消息(JSON)
-----------------|----------------


MQTT承载即时消息报文的Topic，全部以'$IM/'前缀标识，以便Broker对即时消息进行处理。

例如发送给用户1(uid1)的即时消息，MQTT Topic: '$IM/user/uid1'。

### 消息(Message)报文格式

```
{
	id: “akx292akak229”,
	from: “uid1”,
	to: “uid2”,
	type: “chat | grpchat | event”,
	chatid: ‘thread id’,
	body: "ahahah"
  	ts: 1938294828
 }
```
 
type: "text" | "image" | "audio" | "video" | ...


Name            |Type   |Req    |Description
------------------------|-------|-------|------------
id              |string |true   |
thread          |string |true   |
type            |string |true   | chat, grpchat
thread          |string |true   | thread id
content         |object |true   | message content
ts              |long   |true   | timestamp of broker

### 现场(Presence)报文格式

```
{
    "id": 'jack',
    "from": "jack",
    "type": "online",
    "show": "online",
    "status": "I am online",
    "status_time": "10:55",
    "vcard": {}
}
```

Name            |Type   |Req    |Description
------------------------|-------|-------|------------
id              |string |true   |
from            |string |true   | presence from
type            |string |true   | online | offline | subscribe | subscribed | unsubscribe | unsubscribed
show            |string |true   | away | chat | dnd 
status          |string |true   |
vcard           |object |false  |

## SlimChat对象定义

### 好友对象(Buddy)

```
{
    'id' : 'ahhaahkakkda',
    'name' : 'jack',
    'nick' : 'Jack',
    'group' : 'friend',
    'avatar' : 'http://gravatar.com/kakdakddj28dj.png'
}
```

Name            |Type   |Req    |Description
------------------------|-------|-------|------------
id              |string |true   |
name            |string |true   |
nick            |string |true   |
group           |string |true  
avatar          |string |true 

### 群组对象(Room)

```
{
    'id' : 'ahhaahkakkda',
    'name' : 'room',
    'nick' : 'Room1',
    'group' : 'RD',
    'avatar' : 'http://gravatar.com/kakdakddj28dj.png'
}
```

Name            |Type   |Req    |Description
------------------------|-------|-------|------------
id              |string |true   |
name            |string |true   |
nick            |string |true   |
group           |string |true  
avatar          |string |true 

### 端点对象(Client)

```    
{
    'appid': 'livehub.io',
    'deviceid': 'android-2922',
    'clientid': 'livehub-android-akakkdk',
    'ticket': 'uuidticket'
}
```

Name            |Type   |Req    |Description
----------------|-------|-------|------------
type            |string |true   | web/mac/windows/linux/android/ios
appid           |string |true   |
deviceid        |string |true   |
clientid        |string |true   |
ticket          |string |true   |
        

## SlimChat业务流程

### 用户认证

![User Authentication](http://slimchat.io/img/UserAuth.png)

```

title User Authentication Sequence

ClientA->AppServer: HTTP POST /login
AppServer-->ClientA: Return Auth Token
ClientA->Broker: Connect with Auth Token
Broker-->ClientA: Auth Accept
ClientA<-->Broker: Send/Receive Messages
```

### 用户同步业务服务器

POST /online

参数:

Last message id:
Roster version

返回:

配置、好友、群组变更


### 客户端连接消息服务器

MQTT Connect(clientId, username, token)


### 客户端发送消息

MQTT Publish(Message)

Topic: 

chat/node/user1
chat/room/room1


### 客户端接收消息

MQTT onMessage(Message)


### 好友管理

HTTP GET /roster 同步好友列表

HTTP POST /roster 添加好友

HTTP PUT /roster/${id} 更新好友备注

HTTP DELETE /roster/${id} 删除好友

HTTP POST /roster/${id}/block 屏蔽好友

HTTP POST /roster/${id}/unblock 接触屏蔽

### 群组管理

HTTP GET /rooms 同步群组列表

HTTP POST /rooms/ 创建群组

HTTP GET /rooms/${room}/members/ 同步群组成员

HTTP POST /rooms/${room}/members/ 邀请加入群组

HTTP POST /rooms/${room}/join 同意加入群组

HTTP POST /rooms/${room}/leave 退出群组

HTTP POST /rooms/${room}/block 屏蔽群组

HTTP POST /rooms/${room}/unblock 解除屏蔽群组

### 现场状态(Presence)

TODO: Broker集成Roster数据，用户上下线时广播Presence消息。

### 发送消息

MQTT Publish发送到消息服务器。

### 发送状态

MQTT Publish发送到消息服务器。

### 发送图片、音频、视频

HTTP POST /attachements/（发送图片、音频、视频）

HTTP GET  /attachements/ (读取图片、音频、视频)


## Implementation

Erlang with emqttd broker


## Author
	
Feng@emqtt.io


## Referrence

WhatsApp

XMPP

## 讨论

### Last MessageId保存在服务端Session?

