#!/bin/bash
dir_apps='/srv/hd/Relocator/'
apps=`ls "$dir_apps" | grep app`   
echo $apps
while true; do
USERS=99999
  for var2 in $apps; do
    pid=`sudo lsof | grep ^R | grep "$dir_apps" | awk '{print $2, $9}' | grep $var2'$' |awk '{print $1}' `
    if [ -z "$pid" ]; then     
      USERS=0
      APP=$var2
    else
      num=`sudo netstat -p | grep "$pid" | wc -l`
      echo 'PID: '$pid'; App: '$var2'; Users: '$num  
      if [ "$num" -lt "$USERS" ]; then
          USERS=$num
          APP=$var2
      fi
    fi 
  done
  echo 'App: '$APP'; Users: '$USERS  

  echo $APP >  $dir_apps'/min_user'
  sleep 1
done

