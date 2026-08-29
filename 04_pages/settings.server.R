# 04_pages/settings.server.R — 设置

settings_server = \(id, con, rv) {
  moduleServer(id, \(input, output, session) {
    ns = session$ns

    fill_inputs = \(s) {
      updateNumericInput(session, "starting_cash", value = s$starting_cash)
      updateNumericInput(session, "max_assets", value = s$max_assets)
      updateNumericInput(session, "commission_rate", value = s$commission_rate)
      updateNumericInput(session, "stamp_duty", value = s$stamp_duty)
      updateNumericInput(session, "slip_factor", value = s$slip_factor)
      updateNumericInput(session, "spread_adjust", value = s$spread_adjust)
      updateNumericInput(session, "flat_commission", value = s$flat_commission)
      updateNumericInput(session, "per_share_commission", value = s$per_share_commission)
    }

    fill_inputs(isolate(rv$settings))

    observeEvent(input$save, {
      set_setting(con, "starting_cash", input$starting_cash)
      set_setting(con, "max_assets", input$max_assets)
      set_setting(con, "commission_rate", input$commission_rate)
      set_setting(con, "stamp_duty", input$stamp_duty)
      set_setting(con, "slip_factor", input$slip_factor)
      set_setting(con, "spread_adjust", input$spread_adjust)
      set_setting(con, "flat_commission", input$flat_commission)
      set_setting(con, "per_share_commission", input$per_share_commission)
      rv$settings = load_settings(con)
      showNotification("设置已保存", type = "message")
    })

    observeEvent(input$reset, {
      dbExecute(con, "DELETE FROM settings")
      rv$settings = load_settings(con)
      fill_inputs(rv$settings)
      showNotification("已恢复默认值", type = "message")
    })
  })
}
