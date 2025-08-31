# Document Title
        90 +                  log::info!("Auto-detecting best connection mode for server: {}", config.name);
        91 +                  // Try Server Mode first (fastest), fallback to SSH Tunnel, then SSH Direct
        92 +                  if let Ok(conn) = connection::server_mode::ServerModeConnection::new(config.clone()).await {
        93 +                      log::info!("Auto-selected Server Mode for server: {}", config.name);
        94 +                      Arc::new(conn)
        95 +                  } else if let Ok(conn) = connection::ssh_tunnel::SSHTunnelConnection::new(config.clone()).await {
        96 +                      log::info!("Auto-selected SSH Tunnel Mode for server: {}", config.name);
        97 +                      Arc::new(conn)
        98 +                  } else {
        99 +                      log::info!("Auto-selected SSH Direct Mode for server: {}", config.name);
       100 +                      Arc::new(connection::ssh::SSHConnection::new(config.clone()).await?)
       101 +                  }

