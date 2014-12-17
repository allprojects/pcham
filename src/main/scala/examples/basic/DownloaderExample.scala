package examples.basic

import pack1._

object DownloaderExample extends App {
  
  //events enforcing a given order ov execution
  val init = new Event("init");
  val k1 = new Event("k1");
  val k2 = new Event("k2");
  val k3 = new Event("k3");
  val k4 = new Event("k4");
  val k5 = new Event("k5");
  
  //events describing the current state of a download
  val padding = new Event("padding");
  val loading = new Event("loading");
  val progress = new Event("progress");
  val finished = new Event("finished");
  val abborted = new Event("abborted");
  
  Site(List( // Handlers
        Handler(PRE{init},List())(POST{k1},List()){}
        ))(List()){
    println("Download cycle started")
  }
  
  Site(List( // Handlers
        Handler(PRE{k1},List())(POST{k2},List(padding)){}
        ))(List()){
    println("Download padding")
  }
  Site(List( // Handlers
        Handler(PRE{k2},List(padding))(POST{k3},List(loading)){ println()}
        ))(List()){
    println("Download starting")
  }
  Site(List( // Handlers
        Handler(PRE{k3},List(loading ))(POST{k4},List(progress)){ println()}
        ))(List()){
    println("Download progressed")
  }
  
  Site(List( // Handlers
        Handler(PRE{k4},List(loading ))(POST{k4},List(progress)){ println( "Download progressing")},
        Handler(PRE{k4},List(finished  ))(POST{k5},List()){ println("Download cycle finished")},
        Handler(PRE{k4},List(abborted ))(POST{k5},List()){ println(" Download cycle abborted")}
        ))(List()){
  }

  
  init!
  
  Monitor.run
}