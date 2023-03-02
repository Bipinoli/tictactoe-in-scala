package com.olibipin
package observable

trait Subject[A]  {
  var lastValue: Option[A] = None
  var listeners: Array[Listener[A]] = Array()
  def subscribe(listener: Listener[A]): Unit = {
    listeners = listeners :+ listener
    lastValue match {
      case Some(value) => listener.notify(value)
    }
  }
  def update(value: A): Unit = {
    listeners.foreach(listener => listener.notify(value))
    lastValue = Some(value)
  }
}
