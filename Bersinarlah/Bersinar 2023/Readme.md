# read me

## how to build

_login_ dulu

```
docker login
```

```
docker build -t ikanx101/shiny-am-jot .
```

```
sudo docker tag ikanx101/shiny-am-jot:latest ikanx101/shiny-am-jot:latest
```

```
sudo docker push ikanx101/shiny-am-jot:latest
```

## how to run

```
docker run -p 3232:3838 -d --name am-jotform_converter --restart unless-stopped ikanx101/shiny-am-jot:latest
```
