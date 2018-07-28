package org.maps.clustering.tree

import org.maps.clustering.clustering.{Bounds, Item, Tree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object QuadTree {

  /**
    * Maximum number of elements to store in a quad before splitting.
    */
  private val MAX_ELEMENTS = 50
  /**
    * Maximum depth.
    */
  private val MAX_DEPTH = 40

  trait TreeNode[T <: Item] {
    def add(item: T): TreeNode[T]

    def remove(item: T): Unit

    def search(searchBounds: Bounds, results: ArrayBuffer[T]): Unit
  }

  case class LeafNode[T <: Item](bounds: Bounds, depth: Int) extends TreeNode[T] {
    var items: mutable.Set[T] = mutable.Set.empty[T]

    override def add(item: T): TreeNode[T] = {
      val point = item.point
      if (this.bounds.contains(point.x, point.y))
        insert(point.x, point.y, item)
      else
        this
    }

    def insert(x: Double, y: Double, item: T): TreeNode[T] = {
      items += item
      if (items.size > MAX_ELEMENTS && depth < MAX_DEPTH)
        split()
      else
        this
    }

    private def split(): TreeNode[T] = {
      var node = new BranchNode[T](bounds, depth)
      items.foreach(node.add)
      node
    }

    override def remove(item: T): Unit = {
      val point = item.point
      if (this.bounds.contains(point.x, point.y))
        items -= item
    }

    override def search(searchBounds: Bounds, results: ArrayBuffer[T]): Unit = {
      if (this.bounds.intersects(searchBounds)) {
        var updatedResults = results
        if (searchBounds.contains(bounds)) {
          updatedResults ++= items
        } else {
          for (item <- items) {
            if (searchBounds.contains(item.point))
              updatedResults += item
          }
        }
      }
    }
  }

  case class BranchNode[T <: Item](bounds: Bounds, depth: Int) extends TreeNode[T] {
    private var quadrants: mutable.IndexedSeq[TreeNode[T]] = mutable.IndexedSeq[TreeNode[T]](
      LeafNode(Bounds(bounds.minX, bounds.midX, bounds.minY, bounds.midY), depth + 1),
      LeafNode(Bounds(bounds.midX, bounds.maxX, bounds.minY, bounds.midY), depth + 1),
      LeafNode(Bounds(bounds.minX, bounds.midX, bounds.midY, bounds.maxY), depth + 1),
      LeafNode(Bounds(bounds.midX, bounds.maxX, bounds.midY, bounds.maxY), depth + 1)
    )

    override def add(item: T): TreeNode[T] = {
      val point = item.point
      if (this.bounds.contains(point.x, point.y))
        insert(point.x, point.y, item)
      else
        this
    }

    private def insert(x: Double, y: Double, item: T): TreeNode[T] = {
      if (y < bounds.midY) {
        if (x < bounds.midX) { // top left
          quadrants(0) = quadrants(0).add(item)
        } else { // top right
          quadrants(1) = quadrants(1).add(item)
        }
      } else {
        if (x < bounds.midX) { // bottom left
          quadrants(2) = quadrants(2).add(item)
        } else {
          quadrants(3) = quadrants(3).add(item)
        }
      }
      this
    }

    override def remove(item: T): Unit = {
      val point = item.point
      if (this.bounds.contains(point.x, point.y))
        remove(point.x, point.y, item)

    }

    private def remove(x: Double, y: Double, item: T): Unit = {
      if (y < bounds.midY) {
        if (x < bounds.midX) { // top left
          this.quadrants(0).remove(item)
        } else { // top right
          this.quadrants(1).remove(item)
        }
      } else {
        if (x < bounds.midX) { // bottom left
          this.quadrants(2).remove(item)
        } else {
          this.quadrants(3).remove(item)
        }
      }
    }

    override def search(searchBounds: Bounds, results: ArrayBuffer[T]): Unit = {
      if (this.bounds.intersects(searchBounds)) {
        for (quad <- quadrants) {
          quad.search(searchBounds, results)
        }
      }
    }
  }

}

class QuadTree[T <: Item](val bounds: Bounds, val depth: Int) extends Tree[T] {

  import QuadTree._

  private var root = BranchNode[T](bounds, 0)

  def this(bounds: Bounds) {
    this(bounds, 0)
  }

  def add(item: T): Unit = {
    this.root.add(item)
  }

  def remove(item: T): Unit = {
    this.root.remove(item)
  }

  def clear(): Unit = {
    this.root = BranchNode[T](bounds, 0)
  }

  def search(searchBounds: Bounds): Seq[T] = {
    val results = ArrayBuffer[T]()
    this.root.search(searchBounds, results)
    results
  }

}