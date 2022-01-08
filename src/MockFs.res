@module external mock: {..} => unit = "mock-fs"

@module("mock-fs") external restore: unit => unit = "restore"
