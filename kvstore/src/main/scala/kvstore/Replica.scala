package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case object PersistAgain

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
 
  context.system.scheduler.schedule(100.millis, 100.millis, context.self, PersistAgain)

  override def supervisorStrategy = OneForOneStrategy() {
    case _ : PersistenceException => Restart
  }

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  
  var persistlist = Map.empty[Long, (ActorRef, Option[Persist], Set[ActorRef])]

  var _seqCounter = 0L
  
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  var expected = 0L
  
  val persistence = context.actorOf(persistenceProps)


  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    


    case Insert(key, value, id) => {
      kv += key -> value
      persistPrimary(id, key, Some(value))  
    }
    case Remove(key, id) => {
      kv -= key      
      persistPrimary(id, key, None)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Replicated(key, id) => {
      persistlist.get(id)
      .map {
        repl => { 
          persistlist += id -> (repl._1, repl._2, repl._3 + sender)
       }
      }
       persistlist.get(id)
       .map {
         repl => 
           val (replicatee, persist, repls) = repl
           persist match {
             case None if replicators.forall(repls.contains(_)) => {
               replicatee ! OperationAck(id)
               persistlist -= id
             }
             case _ =>
           }
      }
    }

   case Persisted(key, id) => {
      persistlist.get(id)
      .map {
        repl => {
          val (replicatee, persist, replicas) = repl
          persistlist += id -> (replicatee, None, replicas)

        }
      }
      persistlist.get(id)
       .map {
         repl => 
           val (replicatee, persist, repls) = repl
           persist match {
             case None if replicators.forall(repls.contains(_)) => {
               replicatee ! OperationAck(id)
               persistlist -= id
             }
             case _ =>
           }
      }
    }
 
    
  }
  
  



  /* TODO Behavior for the replica role. */
  val replica: Receive = { 
    
    case Snapshot(key, valueoption, seq) => {
      if (seq > expected) () // do nothing
      else if (seq < expected) { 
        sender ! SnapshotAck(key, seq)
      } else {
        valueoption match {
          case Some(v) => kv += key -> v
          case None => kv -= key
        }
        val persist = Persist(key, valueoption, seq)
        persistlist += seq -> (sender, Some(persist), Set.empty[ActorRef])
        persistence ! persist  
      }

    }

    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    
   case Persisted(key, id) => {
     persistlist.get(id)
     .map(
       repl => {
         val (requestee, persist, _) = repl
         persist.map(p => requestee ! SnapshotAck(key, p.id))
         expected += 1
       })
       persistlist -= id
     }
  
  

   case PersistAgain => {
     persistlist.foreach( repl => {
       val (_, (_, p, _)) = repl
       p.map(persistence ! _)
     })
   }
 }
  
  arbiter ! Join
  
  def persistPrimary(id: Long, key: String, valueoption: Option[String]) = {
    val persist = Persist(key, valueoption, id)
    persistlist += id -> (sender, Some(persist), Set.empty[ActorRef])
    persistence ! persist

  }

}

