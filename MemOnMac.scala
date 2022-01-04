object MemOnMac {

  def getFreeMem() = {
    import java.io._
    val p = Runtime.getRuntime().exec("vm_stat")
    val i = new BufferedReader(new InputStreamReader(p.getInputStream()))
    val l1 = i.readLine()
    if (l1 != null && l1.contains("page size of 4096 bytes")) {
      val l2 = i.readLine()
      if (l2 != null) {
        "Pages free.*\\b([0-9]+)\\b".r.findFirstMatchIn(l2) match {
          case Some(result) =>
            if (result.groupCount > 0) {
              val totalBytes = result.group(1).toLong * 4096
              val totalKb = totalBytes / 1024
              val totalMegas = totalKb / 1024
              val totalGigas = totalMegas / 1024
              s"${totalBytes} bytes, ${totalKb} kb, ${totalMegas} mb, ${totalGigas} gb"
            } else {
              "unknown"
            }
          case None =>
            "unknown"
        }
      } else {
        "unknown"
      }
    } else {
      "unknown"
    }
  }

}
