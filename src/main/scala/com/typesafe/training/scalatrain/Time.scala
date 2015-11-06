package com.typesafe.training.scalatrain

object Time {

  def fromMinutes(minutes: Int): Time = new Time(minutes / 60, minutes % 60)
}

case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time] {
  require(hours >= 0 && hours < 24, "hours must be within 0..23!")
  require(minutes >= 0 && minutes < 60, "minutes must be within 0..59!")

  lazy val asMinutes: Int = 60 * hours + minutes

  override val toString = "%02d:%02d".format(hours, minutes)

  def -(that: Time) = minus(that)

  def minus(that: Time): Int = {
    require(that != null, "that must not be null!")
    asMinutes - that.asMinutes
  }

  override def compare(that: Time) = {
    require(that != null, "that must not be null!")
    this.asMinutes - that.asMinutes
  }
}
