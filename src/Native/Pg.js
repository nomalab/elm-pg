const _nomalab$elm_pg$Native_Pg = function () {
  let helpers
  const pg = require('pg')
  const types = pg.types

  let nextId = 1

  const id = Symbol('elm-pg-id')
  const settings = { pool: {}, client: {} }

  function catchError(e) {
    console.log('ERROR -------------------------------------------------------')
    console.log(require('util').inspect(e, { depth: null }));
    console.log('-------------------------------------------------------------')
    throw e
  }

  function getId() {
    if (nextId > 2147483640) {
      nextId = 1
    }
    return ++nextId
  }

  function equals(a, b) {
    return a[id] === b[id]
  }

  function initHelpers() {
    // hack to bypass the fact you cannot sort native imports in the final JS
    if (!helpers) {
       helpers = _pauldijou$elm_kernel_helpers$Native_Kernel_Helpers
    }
  }

  function normalizeResult(result) {
    console.log('---------------------------------------------------------------------------')
    console.log('normalizeResult')
    console.log('---------------------------------------------------------------------------')
    console.log(Array.isArray(result.rows))
    console.log(require('util').inspect(result, { depth: null }));
    // result.rows = helpers.list.fromArray(result.rows)
    result.fields = helpers.list.fromArray(result.fields)
    return result
  }


  // ---------------------------------------------------------------------------
  // POOL
  // ---------------------------------------------------------------------------

  function poolInit(messages) {
    initHelpers()
    return helpers.task.fromCallback(succeed => {
      settings.pool.messages = messages
      succeed()
    })
  }

  function poolSetup(sendToSelf) {
    initHelpers()
    return helpers.task.fromCallback(succeed => {
      settings.pool.sendToSelf = sendToSelf
      succeed()
    })
  }

  function poolCreate(clientSettings, poolSettings) {
    const createSettings = {}
    for (var key in clientSettings) { createSettings[key] = clientSettings[key] }
    for (var key in poolSettings)   { createSettings[key] = poolSettings[key] }

    const pool = new pg.Pool(createSettings)
    pool[id] = getId()
    return poolEvents(pool)
  }

  function poolConnect(pool) {
    return helpers.task.fromPromise(() => pool.connect().then(clientEvents).catch(catchError))
  }

  function poolQuery(query, pool) {
    return helpers.task.fromPromise(() => pool.query(query).then(normalizeResult).catch(catchError))
  }

  function poolEnd(pool) {
    return helpers.task.fromPromise(() => pool.end().catch(catchError))
  }

  function poolTotalCount(pool) {
    return pool.totalCount
  }

  function poolIdleCount(pool) {
    return pool.idleCount
  }

  function poolWaitingCount(pool) {
    return pool.waitingCount
  }

  function poolSend(msg) {
    if (settings.pool.sendToSelf) {
      helpers.task.rawSpawn(settings.pool.sendToSelf(msg))
    }
  }

  function poolEvents(pool) {
    pool.on('connect', client => {
      // Whenever the pool establishes a new client connection to the PostgreSQL
      // backend it will emit the connect event with the newly connected client.
      // This presents an opportunity for you to run setup commands on a client.
      poolSend(settings.pool.messages.connect({ id: id, client: client }))
    })

    pool.on('acquire', client => {
      // Whenever the a client is checked out from the pool the pool will emit
      // the acquire event with the client that was acquired.
      poolSend(settings.pool.messages.acquire({ id: id, client: client }))
    })

    pool.on('error', (error, client) => {
      // When a client is sitting idly in the pool it can still emit errors
      // because it is connected to a live backend
      poolSend(settings.pool.messages.error({ id: id, client: client, error: error }))
    })

    pool.on('remove', client => {
      // Whenever a client is closed & removed from the pool the pool will emit
      // the remove event.
      poolSend(settings.pool.messages.remove({ id: id, client: client }))
    })

    return pool
  }


  // ---------------------------------------------------------------------------
  // CLIENT
  // ---------------------------------------------------------------------------

  function clientInit(messages) {
    initHelpers()
    return helpers.task.fromCallback(succeed => {
      settings.client.messages = messages
      succeed()
    })
  }

  function clientSetup(sendToSelf) {
    initHelpers()
    return helpers.task.fromCallback(succeed => {
      settings.client.sendToSelf = sendToSelf
      succeed()
    })
  }

  function clientCreate(settings) {
    return clientEvents(new pg.Client(settings))
  }

  function clientConnect(client) {
    return helpers.task.fromPromise(() => client.connect().catch(catchError))
  }

  function clientQuery(query, client) {
    return helpers.task.fromPromise(() => {
      client.query(query).then(normalizeResult).catch(catchError)
    })
  }

  function clientRelease(client) {
    return helpers.task.fromCallback(succeed => {
      client.release()
      succeed()
    })
  }

  function clientEnd(client) {
    return helpers.task.fromPromise(() => client.end().catch(catchError))
  }

  function clientSend(msg) {
    if (settings.client.sendToSelf) {
      helpers.task.rawSpawn(settings.client.sendToSelf(msg))
    }
  }

  function clientEvents(client) {
    if (client[id]) { return client }
    client[id] = getId()

    client.on('error', error => {
      // When the client is in the process of connecting, dispatching a query,
      // or disconnecting it will catch and foward errors from the PostgreSQL
      // server to the respective client.connect client.query or client.end
      // callback/promise; however, the client maintains a long-lived connection
      // to the PostgreSQL back-end and due to network partitions, back-end
      // crashes, fail-overs, etc the client can (and over a long enough time
      // period will) eventually be disconnected while it is idle. To handle
      // this you may want to attach an error listener to a client to catch
      // errors.
      clientSend(settings.client.messages.error({ id: id, error: error }))
    })

    client.on('end', () => {
      // When the client disconnects from the PostgreSQL server it will emit an
      // end event once.
      clientSend(settings.client.messages.end({ id: id }))
    })

    client.on('notification', notification => {
      // Used for listen/notify events:
      if (notification.payload === null || notification.payload === undefined) {
        notification.payload = helpers.maybe.nothing
      } else {
        notification.payload = helpers.maybe.just(notification.payload)
      }
      clientSend(settings.client.messages.notification({ id: id, notification: notification }))
    })

    client.on('notice', notice => {
      // Used to log out notice messages from the PostgreSQL server.
      clientSend(settings.client.messages.notice({ id: id, notice: notice }))
    })

    return client
  }


  // ---------------------------------------------------------------------------
  // HELPERS
  // ---------------------------------------------------------------------------

  function setTypeParser(oid, parser) {
    return helpers.task.fromCallback(succeed => {
      types.setTypeParser(oid, parser)
      succeed()
    })
  }

  function unprefix(prefix, row) {
    if (Array.isArray(row)) { return row }

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
    // Pool
    poolInit: poolInit,
    poolSetup: poolSetup,
    poolCreate: F2(poolCreate),
    poolConnect: poolConnect,
    poolQuery: F2(poolQuery),
    poolEnd: poolEnd,
    poolTotalCount: poolTotalCount,
    poolIdleCount: poolIdleCount,
    poolWaitingCount: poolWaitingCount,
    // Client
    clientInit: clientInit,
    clientSetup: clientSetup,
    clientCreate: clientCreate,
    clientConnect: clientConnect,
    clientQuery: F2(clientQuery),
    clientRelease: clientRelease,
    clientEnd: clientEnd,
    // Helpers
    setTypeParser: F2(setTypeParser),
    unprefix: F2(unprefix),
    equals: F2(equals),
    dependency: function () { return '' }
  }
}()
