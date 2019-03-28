package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    // 1、 得到wd
    val wd = state.wd

    // 2、 得到绝对路径
    val absolutePath =
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    // 3、 做一些检查，不支持rm / 的操作
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Not support remove root yet!")
    else
      doRm(state, absolutePath)
  }

  def doRm(state: State, path: String): State = {

    /*
      ["a", "b"]
      ?empty false
        nextDirectory = /a
        newNextDirectory = rmHelper(/a, ["b"])
                                ?empty false
                                ?tail.isEmpty true => new /a(删除了/b)
        root.replaceEntry("a", new /a) => new root
     */
    def rmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDirectory   // 没有东西要删除，rm失败，返回本身，因此结构不会更新，newRoot和oldRoot会相等。
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDirectory = currentDirectory.findEntry(path.head)
        if (!nextDirectory.isDirectory) currentDirectory   // 下一个要进入的文件夹不是文件夹，则rm失败，返回当前文件夹，整个结构没有发生更改。
        else {
          val newNextDirectory = rmHelper(nextDirectory.asDirectory, path.tail)
          if (newNextDirectory == nextDirectory) currentDirectory  // rmHelper递归的返回删除执行之后的文件夹，一层一层的往上更新结构。如果删除执行之后的文件夹，和执行之前的一样，说明删除命令执行失败，则直接返回当前文件夹，不更新整个结构。
          else currentDirectory.replaceEntry(path.head, newNextDirectory)  // 更新结构，把当前的文件夹替换为内容更新过的文件夹
        }
      }
    }

    // 4、 找到要删除的entry
    // 5、 更新整个结构，跟在mkdir里做的一样

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if (newRoot == state.root)
      state.setMessage(path + ": no such file or directory!")   // 如果返回的新的根节点和原来的一样，则代表rm命令执行失败了。即结构没有更新，说明没有发生删除。
    else
    // 更新状态为，在新的根节点，即新的结构下的原本所在的wd的位置。为了方便，overload一个findDescendant方法，接收root的relative path作为参数。
      State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))


  }

}
