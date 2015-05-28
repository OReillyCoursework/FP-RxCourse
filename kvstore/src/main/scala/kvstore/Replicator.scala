package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  import scala.collection.mutable.Queue
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var acks = Map.empty[Long, (ActorRef, Replicate)]

  var _seqCounter = 0L
  
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  var expected = 0L

  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {

    case repl @ Replicate(key, valueoption, id) => {
      val seq = nextSeq
      acks += seq -> (sender, repl)
      replica ! Snapshot(key, valueoption, id)  
      
    }

    case SnapshotAck(key, seq) => {
      acks.get(seq)  
      .map{
        pair => {
          val (leader, repl) = pair 
          replica ! Replicated(key, repl.id)
        }
      }
      acks -= seq
    }

  }

  

}
