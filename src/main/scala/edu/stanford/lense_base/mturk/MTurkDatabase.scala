package edu.stanford.lense_base.mturk

import java.sql.{Connection, DriverManager}

import scala.collection.mutable.ListBuffer

/**
 * Created by keenon on 5/18/15.
 *
 * It seems the responsible thing to do to keep state for all of our workers, and not accidentally miss payment etc b/c
 * of unforseen crashes and connection loss
 */
object MTurkDatabase {
  // Warm up the driver
  Class.forName("org.h2.Driver")
  // Create a connection
  lazy val conn : Connection = DriverManager.
    getConnection("jdbc:h2:./src/main/resources/mturk/turk_state;mode=mysql;", "sa", "")

  // Initialize database
  executeSQL("CREATE TABLE IF NOT EXISTS `workers` " +
    "(`workerId` VARCHAR(255) not NULL,"+
    "`queriesAnswered` INTEGER," +
    "`connectionDuration` LONG," +
    "`outstandingBonus` DOUBLE,"+
    "`currentlyConnected` BOOLEAN,"+
    "`assignmentId` VARCHAR(255),"+
    "PRIMARY KEY ( `workerId` ))")

  def updateOrCreateWorker(turkState : MTurkDBState) : Unit = {
    val stmt = conn.prepareStatement("INSERT INTO "+
      "`workers`(`workerId`,`queriesAnswered`,`connectionDuration`,`outstandingBonus`,`currentlyConnected`, `assignmentId`) "+
      "VALUES (?,?,?,?,?) "+
      "ON DUPLICATE KEY UPDATE "+
      "`queriesAnswered`=?,"+
      "`connectionDuration`=?,"+
      "`outstandingBonus`=?,"+
      "`currentlyConnected`=?,"+
      "`assignmentId`=?")
    stmt.setString(1, turkState.workerId)
    stmt.setInt(2, turkState.queriesAnswered)
    stmt.setLong(3, turkState.connectionDuration)
    stmt.setDouble(4, turkState.outstandingBonus)
    stmt.setBoolean(5, turkState.currentlyConnected)
    stmt.setString(6, turkState.assignmentId)
    stmt.setInt(7, turkState.queriesAnswered)
    stmt.setLong(8, turkState.connectionDuration)
    stmt.setDouble(9, turkState.outstandingBonus)
    stmt.setBoolean(10, turkState.currentlyConnected)
    stmt.setString(11, turkState.assignmentId)
    stmt.executeUpdate()
    conn.commit()
  }

  def getAllWorkers : List[MTurkDBState] = {
    val stmt = conn.prepareStatement("SELECT * from `workers`")
    val rset = stmt.executeQuery()
    val lbuffer = ListBuffer[MTurkDBState]()
    while (rset.next()) {
      lbuffer.+=(MTurkDBState(
        rset.getString("workerId"),
        rset.getInt("queriesAnswered"),
        rset.getLong("connectionDuration"),
        rset.getDouble("outstandingBonus"),
        rset.getBoolean("currentlyConnected"),
        rset.getString("assignmentId")
      ))
    }
    lbuffer.toList
  }

  def getWorker(workerId : String) : MTurkDBState = {
    val stmt = conn.prepareStatement("SELECT * from `workers` WHERE `workerId`=?")
    stmt.setString(1, workerId)
    val rset = stmt.executeQuery()
    if (rset.next()) {
      MTurkDBState(
        rset.getString("workerId"),
        rset.getInt("queriesAnswered"),
        rset.getLong("connectionDuration"),
        rset.getDouble("outstandingBonus"),
        rset.getBoolean("currentlyConnected"),
        rset.getString("assignmentId")
      )
    }
    else {
      null
    }
  }

  def executeSQL(sql : String) : Unit = {
    val stmt = conn.prepareStatement(sql)
    stmt.execute()
  }

  def close() = {
    conn.close()
  }

  def main(args : Array[String]) : Unit = {
    val state = MTurkDBState("test", 3, 16000L, 3.01, currentlyConnected = true)
    MTurkDatabase.updateOrCreateWorker(state)
    println(MTurkDatabase.getWorker("test"))
    MTurkDatabase.close()
  }
}

case class MTurkDBState(workerId : String,
                        queriesAnswered : Int = 0,
                        connectionDuration : Long = 0,
                        outstandingBonus : Double = 0.0,
                        currentlyConnected : Boolean = true,
                        assignmentId : String = "")
