package org.maps.clustering.tree

import org.maps.clustering.clustering._
import org.maps.clustering.tree.HilbertTree._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object HilbertTree {

  private val MAX_ELEMENTS = 50
  private val MAX_DEPTH = 40


  trait Node[T <: Item] {
    def add(item: T): Node[T]

    def remove(item: T): Unit

    def clear(): Unit

    def size(): Int

    def search(hash: GeoHash, results: ArrayBuffer[T], bounds: Option[Bounds])
  }

  case class BranchNode[T <: Item](hash: GeoHash, maxItems: Int, maxBits: Int) extends Node[T] {
    var nodes: mutable.IndexedSeq[Node[T]] =
      mutable.IndexedSeq[Node[T]](
        LeafNode[T](hash.withSuffix(0x0), maxItems, maxBits),
        LeafNode[T](hash.withSuffix(0x1), maxItems, maxBits),
        LeafNode[T](hash.withSuffix(0x2), maxItems, maxBits),
        LeafNode[T](hash.withSuffix(0x3), maxItems, maxBits)
      )

    def add(item: T): Node[T] = {
      val itemHash = item.hash
      val truncated = itemHash.truncatePrefix(hash)
      nodes(truncated.cell) = nodes(truncated.cell).add(item)
      this
    }

    def remove(item: T): Unit = {
      val itemHash = item.hash
      val truncated = itemHash.truncatePrefix(hash)
      nodes(truncated.cell).remove(item)
    }

    def clear(): Unit = {
      nodes.foreach(_.clear())
    }

    def search(searchHash: GeoHash, results: ArrayBuffer[T], bounds: Option[Bounds]): Unit = {
      nodes.foreach {
        case l: LeafNode[T] if l.hash.isCommonPrefix(searchHash) => l.search(searchHash, results, bounds)
        case b: BranchNode[T] if b.hash.isCommonPrefix(searchHash) => b.search(searchHash, results, bounds)
        case _ =>
      }
    }

    override def size(): Int = this.nodes.foldLeft(0)(_ + _.size())
  }

  case class LeafNode[T <: Item](hash: GeoHash, maxItems: Integer, maxBits: Int) extends Node[T] {
    var items: Set[T] = Set.empty[T]

    def add(item: T): Node[T] = {
      if (items.size < maxItems || hash.validBits >= maxBits) {
        items += item
        this
      } else {
        split(item)
      }
    }

    private def split(item: T): Node[T] = {
      val branch = BranchNode[T](hash, maxItems, maxBits)
      this.items.foreach(branch.add)
      branch.add(item)
      branch
    }

    def clear(): Unit = {
      items = Set.empty
    }

    def remove(item: T): Unit = {
      items -= item
    }

    def search(searchHash: GeoHash, results: ArrayBuffer[T], bounds: Option[Bounds]): Unit = {
      if (searchHash.matchPrefix(this.hash)) {
        results ++= this.items
      } else {
        this.items.foreach { item =>
          if (item.hash.matchPrefix(searchHash) && (bounds.isEmpty || bounds.get.contains(item.point))) {
            results += item
          }
        }
      }
    }

    override def size(): Int = items.size
  }

}

class HilbertTree[T <: Item](maxBits: Int, depth: Int = MAX_DEPTH, maxItems: Int = MAX_ELEMENTS) extends Tree[T] {

  import HilbertTree._

  private var root: BranchNode[T] = new BranchNode[T](GeoHash(0, 0), maxItems, maxBits)

  def add(item: T): Unit = {
    root.add(item)
  }

  def remove(item: T): Unit = {
    root.remove(item)
  }

  def clear(): Unit = {
    root = new BranchNode[T](GeoHash(0, 0), maxItems, maxBits)
  }

  def search(searchHash: GeoHash, bounds: Option[Bounds]): IndexedSeq[T] = {
    val results = ArrayBuffer[T]()
    root.search(searchHash, results, bounds)
    results
  }

  def size(): Int = this.root.size()

}
