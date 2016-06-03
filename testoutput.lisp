package main
--^^^^^^^^^^  source.go

--  source.go
import (
--^^^^^^  source.go
	"log"
--^^^^  source.go
	"net/http"
--^^^^^^^^^  source.go
	"net/http/cookiejar"
--^^^^^^^^^^^^^^^^^^^  source.go
	"os"
--^^^  source.go
	"path"
--^^^^^  source.go
	"runtime"
--^^^^^^^^  source.go
	"time"
--^^^^^  source.go
	"vger/dbHelper"
--^^^^^^^^^^^^^^  source.go
	"vger/download"
--^^^^^^^^^^^^^^  source.go
	"vger/filelock"
--^^^^^^^^^^^^^^  source.go
	"vger/logger"
--^^^^^^^^^^^^  source.go
	"vger/subscribe"
--^^^^^^^^^^^^^^^  source.go
	"vger/thunder"
--^^^^^^^^^^^^^  source.go
	"vger/util"
--^^^^^^^^^^  source.go
)
--  source.go

--  source.go
func init() {
--^^^^^^^^^^^  source.go
	runtime.GOMAXPROCS(runtime.NumCPU() - 1)
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	err := os.Chdir(path.Dir(os.Args[0]))
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	if err != nil {
--^^^^^^^^^^^^^^  source.go
		log.Fatal(err)
--^^^^^^^^^^^^^^  source.go
	}
--  source.go

--  source.go
	logbase := util.ReadConfig("log")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	logger.InitLog("[V'ger]", path.Join(logbase, "vger.log"))
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	jar, _ := cookiejar.New(nil)
--^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	http.DefaultClient.Jar = jar
--^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	//http.DefaultClient.Jar, _ = nativejar.New()
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	util.SaveConfig("shutdown-after-finish", "false")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	//set timeout
--^^^^^^^^^^^^  source.go
	networkTimeout := time.Duration(util.ReadIntConfig("network-timeout")) * time.Second
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	transport := http.DefaultTransport.(*http.Transport)
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	transport.ResponseHeaderTimeout = networkTimeout
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	transport.MaxIdleConnsPerHost = 3
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	thunder.UserName = util.ReadConfig("thunder-user")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	thunder.Password = util.ReadConfig("thunder-password")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	thunder.Gdriveid = util.ReadConfig("gdriveid")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	formats := util.ReadStringSliceConfig("yyets-formats")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	for _, ft := range formats {
--^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
		subscribe.YYetsFormats[ft] = struct{}{}
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	}
--  source.go
	log.Printf("yyets-formats:%v", subscribe.YYetsFormats)
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	go func() {
--^^^^^^^^^^  source.go
		err := thunder.Login(nil)
--^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
		if err != nil {
--^^^^^^^^^^^^^^^  source.go
			log.Print(err)
--^^^^^^^^^^^^^^^  source.go
		}
--^  source.go
	}()
--^^  source.go

--  source.go
	dbHelper.Init("sqlite3", path.Join(util.ReadConfig("dir"), "vger.db"))
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	filelock.DefaultLock, _ = filelock.New("/tmp/vger.db.lock.txt")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go

--  source.go
	download.BaseDir = util.ReadConfig("dir")
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
	download.NetworkTimeout = networkTimeout
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  source.go
}
--  source.go
