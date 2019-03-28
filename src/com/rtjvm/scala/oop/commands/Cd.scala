package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    /*
      cd /a/b/c/d/...   绝对路径（从根目录/开始）
      cd b/c/d/...    相对路径（从当前工作目录开始）

      cd ..
      cd .
     */

    // 可以分成以下几个小步骤：
    // 1、 找到根目录
    val root = state.root
    val wd = state.wd

    // 2、 找到希望cd到的目录的绝对路径（不管输入的是绝对路径还是相对路径）
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    // 3、 根据绝对路径，找到要cd进去的目录
    val destinationDirectory = doFindEntry(root, absolutePath)

    // 4、 根据新的工作目录，改变state
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory!")
    else
      State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry =
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }

    @tailrec    // 因为得是尾部递归的，所以参数中得多加一项result，用来存储中间结果，保持尾部递归结构。
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      /*
        ["a", "b"]
        path.isEmpty? false
          cRT(["b"], result = [] :+ "a" = ["a"])
            path.isEmpty? false
              cRT([], result = ["a"] :+ "b" = ["a", "b"])
                path.isEmpty? true => ["a", "b"]

        ["a", "b", ".."]
        path.isEmpty? false
          cRT(["b", ".."], ["a"])
            ? false
              cRT([".."], ["a", "b"])
                ? false
                ..? true
                  cRT([], ["a"])

       */
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)   // 注意，这里用init方法，获得除了最后一个之前的所有元素，与tail相对应。
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    // 1. tokens
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList

    // 1.5 去除tokens里的.和..
    /*
      ["a", "."] => ["a"]
      ["a", "b", ".", "."] => ["a", "b"]
      ["a", ".."] => []
      ["a", "b", ".."] => ["a"]

     */

    val newTokens = collapseRelativeTokens(tokens, List())

    // 2. 到达正确的地点
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}
