var _nomalab$elm_pg$Native_Pg = function () {
  var UUID_OID = 2950
  var TEXT_OID = 25
  var VARCHAR_OID = 1043
  var TIMESTAMP_OID = 1114 // Timestamp without timezone
  var TIMESTAMPTZ_OID = 1184
  var BIGINT_OID = 20

  var helpers = _pauldijou$elm_kernel_helpers$Native_Kernel_Helpers
  var pg = require('pg')
  var types = pg.types
  types.setTypeParser(TIMESTAMP_OID, parseTimestamp)
  types.setTypeParser(TIMESTAMPTZ_OID, parseTimestamp)
  types.setTypeParser(BIGINT_OID, parseIntRadix10)

  function normalizeOID(oid) {
    if (typeof oid !== 'number') { return null }
    if (isNaN(oid)) { return null }
    return oid
  }

  function parseIntRadix10(i) {
    return parseInt(i, 10)
  }

  function parseTimestamp(t) {
    return (t.replace(' ', 'T')) + 'Z'
  }

  function createPool(settings) {
    return new pg.Pool({
      fullHost: settings.host + ':' + settings.port_ + '/' + settings.database,
      host: settings.host,
      database: settings.database,
      user: settings.user,
      password: settings.password,
      port: settings.port_,
      max: settings.max,
      min: settings.min,
      ssl: settings.ssl,
      idleTimeoutMillis: settings.idleTimeoutMillis
    })
  }

  function connect(pool) {
    return helpers.task.fromPromise(pool.connect)
  }

  function release(client) {
    return helpers.task.fromCallback(succeed => {
      client.release()
      succeed()
    })
  }

  function query(sql, params, client) {
    return helpers.task.fromPromise(() => {
      client.query(sql, params).then(result => {
        result.oid = normalizeOID(result.oid)
        result.rows =  helpers.list.fromArray(result.rows)
        return result
      })
    })
  }

  function unprefix(prefix, row) {
    const result = {}
    let hasValue = false
    for (let key in row) {
      if (key.indexOf(prefix) === 0) {
        result[key.substr(prefix.length)] = row[key]
        if (row[key] !== undefined && row[key] !== null) {
          hasValue = true
        }
      }
    }
    if (!hasValue) {
      return null
    }
    return result
  }

  return {
    createPool: createPool,
    connect: connect,
    release: release,
    query: F3(query),
    unprefix: F2(unprefix)
  }
}()
