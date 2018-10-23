# haskell-p2p

## installation
``` stack install ```

## building 
``` 
bash build.bash
cd tls
bash ./00_generate_rca.bash
bash ./01_generate_crt.bash


```
## running

Terminal #1:
```
cd bin
./server
```

Terminal #2:
```
bash many.clients.tls.bash
```

## view logs of activity
```
cd logs
head 00042.txt
wc -l *
```


### source
https://hk.saowen.com/a/966d5e7048f9d15d593c2369273678b6f5d432b9fdbc41b97f0fc482aa440811


