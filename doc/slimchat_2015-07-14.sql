# ************************************************************
# Sequel Pro SQL dump
# Version 4096
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: 127.0.0.1 (MySQL 5.6.24)
# Database: slimchat
# Generation Time: 2015-07-14 03:32:38 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table slimchat_buddies
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_buddies`;

CREATE TABLE `slimchat_buddies` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `uid` varchar(40) DEFAULT NULL,
  `fid` varchar(40) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table slimchat_histories
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_histories`;

CREATE TABLE `slimchat_histories` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `type` varchar(20) DEFAULT NULL,
  `from` varchar(50) NOT NULL,
  `to` varchar(50) NOT NULL,
  `nick` varchar(20) DEFAULT NULL COMMENT 'from nick',
  `body` text,
  `timestamp` double DEFAULT NULL,
  `acked` tinyint(1) NOT NULL DEFAULT '0',
  `created` date DEFAULT NULL,
  `updated` date DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `slimchat_history_timestamp` (`timestamp`),
  KEY `slimchat_history_to` (`to`),
  KEY `slimchat_history_from` (`from`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;



# Dump of table slimchat_members
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_members`;

CREATE TABLE `slimchat_members` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `room` varchar(60) NOT NULL,
  `uid` varchar(40) NOT NULL,
  `nick` varchar(60) NOT NULL,
  `joined` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `slimchat_member_room_uid` (`room`,`uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;



# Dump of table slimchat_messages
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_messages`;

CREATE TABLE `slimchat_messages` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `send` tinyint(1) DEFAULT NULL,
  `type` varchar(20) DEFAULT NULL,
  `from` varchar(50) NOT NULL,
  `to` varchar(50) NOT NULL,
  `nick` varchar(20) DEFAULT NULL COMMENT 'from nick',
  `body` text,
  `style` varchar(150) DEFAULT NULL,
  `timestamp` double DEFAULT NULL,
  `acked` tinyint(1) NOT NULL DEFAULT '0',
  `created` date DEFAULT NULL,
  `updated` date DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `slimchat_message_timestamp` (`timestamp`),
  KEY `slimchat_message_to` (`to`),
  KEY `slimchat_message_from` (`from`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;



# Dump of table slimchat_rooms
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_rooms`;

CREATE TABLE `slimchat_rooms` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `owner` varchar(40) NOT NULL,
  `name` varchar(60) NOT NULL DEFAULT '',
  `title` varchar(60) NOT NULL DEFAULT '',
  `topic` varchar(60) DEFAULT NULL,
  `url` varchar(100) DEFAULT '#',
  `created` datetime DEFAULT NULL,
  `updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `slimchat_rooms_name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;



# Dump of table slimchat_users
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_users`;

CREATE TABLE `slimchat_users` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `username` varchar(40) NOT NULL DEFAULT '',
  `password` varchar(100) DEFAULT NULL,
  `avatar` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table slimchat_vcards
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimchat_vcards`;

CREATE TABLE `slimchat_vcards` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `uid` int(11) NOT NULL,
  `vcard` text NOT NULL,
  `created` datetime NOT NULL,
  `updated` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table slimpp_settings
# ------------------------------------------------------------

DROP TABLE IF EXISTS `slimpp_settings`;

CREATE TABLE `slimpp_settings` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `uid` varchar(40) NOT NULL DEFAULT '',
  `data` text,
  `created` datetime DEFAULT NULL,
  `updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `slimchat_setting_uid` (`uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;




/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
