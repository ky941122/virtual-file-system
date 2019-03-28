package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists!")
    } else if (name.contains(Directory.SEPARATOR)) {
      // 不允许文件夹名出现类似  a/b 的结构
      state.setMessage(name + " must not contain separators!")
    } else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name!")
    } else {
      doCreateEntry(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doCreateEntry(state: State, name: String): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      /*
        要做的事情是：
        现在有 ..../somedir
                      /a
                      /b
                      要在此文件夹下新加 /d
        则应该重新创建一个somedir instance，然后里面包含/a /b的原来的instance（因为这些文件夹中可能有东西），和新的/d的instance
        并且，对于somedir的parentdir，somedir也是个需要新创建的文件夹，所以同样的，parentdir也需要被新创建
        因此，这是个递归过程。
       */

      if (path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }

      /*
        假设目前文件夹结构为：
        /a/b
          ...(other contents)
          (要新加的文件夹) /e

        updateStructure(root, ["a", "b"], /e)
          => path.isEmpty? false
          => oldEntry = /a
          root.replaceEntry("a", updateStructure(/a, ["b"], /e))
            => path.isEmpty? false
            => oldEntry = /b
            /a.replaceEntry("b", updateStructure(/b, [], /e))
              => path.isEmpty? true => /b.addEntry(/e)
       */
    }

    val wd = state.wd

    // 主要要做4件事情：
    // 1、找出fullpath上的所有directories
    val allDirsInPath = wd.getAllFoldersInPath

    // 2、在当前wd创建新的directory entry
//    val newDir = Directory.empty(wd.path, name)
    val newEntry: DirEntry = createSpecificEntry(state)

    // 3、从root开始，更新整个文件夹结构
    // （文件夹结构是IMMUTABLE的）
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)

    // 4、在新的文件夹结构中，找出新的working directory INSTANCE
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def createSpecificEntry(state: State): DirEntry
}
