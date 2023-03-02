package com.olibipin
package observable

trait Listener[A] {
  def notify(value: A): Unit
}
