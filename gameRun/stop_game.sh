#! /bin/sh 

#DATETIME=`date "+%Y%m%d%H%M%S"` 
echo 关闭游戏节点
stop()
{
echo $1 
PID=`ps -ef| grep $1 | grep ebin | awk '{print $2}'`
echo $PID
if [ "$PID" != 0 ] ; then
	kill 9 $PID
	 DATETIME=`date "+%Y-%m-%d %H:%M:%S"` 
	echo "NOTICE:$DATETIME===================服务节点$1关闭===================="
	return 0
else 
	echo "节点关闭失败"	
	return 1
fi
}



stop snake_world
stop snake_mynet
stop snake_scene
