package exceptions

/**
 * Thrown when trying to change the stratum to a stratum different from its frame parent, if this parent is different
 * from UIParent.
 */
class WrongStratumException(msg: String) extends Throwable
