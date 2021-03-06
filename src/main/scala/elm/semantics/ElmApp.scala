package elm.semantics

trait ElmApp extends App {
  override def delayedInit(body: => Unit): Unit = {
    ELMRuntime.run(super.delayedInit(body))
  }
}
