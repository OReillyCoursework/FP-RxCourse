/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
      case op:Operation => root ! op

      case GC => {
        var newroot = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
        root ! CopyTo(newroot)
        context become garbageCollecting(newroot)
      }

  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {


    case op: Operation => stash() 
    case GC =>
    case CopyFinished => {
      
      root ! PoisonPill
      root = newRoot
      unstashAll()

      context become normal

    }
    
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = { 
  
    case insert @ Insert(requester, id, elm) => {
      if (elm == elem) {
        removed = false
        requester ! OperationFinished(id)

      } else if (elm < elem) {
        subtrees.get(Left) match {
          case Some(left) => left ! insert 
          case None => {
            subtrees = subtrees + (Left -> context.actorOf(BinaryTreeNode.props(elm, initiallyRemoved = false))) 
            requester ! OperationFinished(id)
          }
        }

        } else if (elm > elem) {
          subtrees.get(Right) match {
            case Some(right) => right ! insert
            case None => {
              subtrees = subtrees + (Right -> context.actorOf(BinaryTreeNode.props(elm, initiallyRemoved = false)))
              requester ! OperationFinished(id)  
            }
          }
      }
    }

    
    case checkcontains @ Contains(requester, id, elm) => {
      if (elm < elem) {
        if (subtrees.contains(Left)) subtrees(Left) ! checkcontains
        else requester ! ContainsResult(id, false)
      } else if (elm > elem) {
          if (subtrees.contains(Right)) subtrees(Right) ! checkcontains
          else requester ! ContainsResult(id, false)
      } else requester ! ContainsResult(id, !removed)
       
    }

    case checkremove @ Remove(requester, id, elm) => {
      if (elm < elem) {
        subtrees.get(Left) match {
          case Some(leftnode) => leftnode ! checkremove
          case None => requester ! OperationFinished(id)
        }

        } else if (elm > elem) {
        subtrees.get(Right) match { 
          case Some(rightnode) => rightnode ! checkremove
          case None => requester ! OperationFinished(id)
        }

      } else {
        removed = true
        requester ! OperationFinished(id)
      }
      
    }

    case CopyTo(treeNode) => {
      val nodes = subtrees.values.toSet
      removed match {
        case true if nodes.isEmpty => context.parent ! CopyFinished
        case _ => {
              nodes foreach {_ ! CopyTo(treeNode) }
              context become copying(nodes, removed)
              if (!removed) treeNode ! Insert(self, elem, elem)
              else self ! OperationFinished(elem) 
            }

          }
        }

   case CopyFinished =>  

  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    
    case CopyFinished =>
      val next = expected - sender
      if (next.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        context become normal
      } else {
        context become copying(next, insertConfirmed)
      }

    case OperationFinished(id) => {
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        context become normal
      } else {
        context become copying(expected, true)
      }
    }
   }
}
