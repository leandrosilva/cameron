# -*- encoding: utf-8 -*-

Rack::API.app do
  prefix     "workflow"
  respond_to :json
  
  version "v0.0.1" do
    get "/start" do
        {
          data: { customer_id:           "007",
                  customer_name:         "Leandro Silva",
                  customer_login:        "leandrosilva.codezone",
                  customer_web_info:     { blog:    "http://leandrosilva.com.br",
                                           twitter: "codezone" },
                  customer_billing_info: { prefered_payment_method: "creditcard",
                                           prefered_due_date:       "on 5 of each month" }},
          next:
            [
              { name: "cloud_zabbix",
                url:  "http://localhost:9292/workflow/cloud/zabbix" },
              { name: "cloud_product",
                url:  "http://localhost:9292/workflow/cloud/product" },
              { name: "hosting_zabbix",
                url:  "http://localhost:9292/workflow/hosting/zabbix" },
              { name: "hosting_product",
                url:  "http://localhost:9292/workflow/hosting/product" },
              { name: "sqlserver_zabbix",
                url:  "http://localhost:9292/workflow/sqlserver/zabbix" }
            ]
        }
    end
    
    get "/cloud/zabbix" do
      {
        data: { cluster_info: "up and running without any problem",
                server_info:  { ip: "192.02.12.10.12",
                                vlan: "vl001",
                                last_stop_at: "01/02/2011" }},
        next: []
      }
    end
    
    get "/cloud/product" do
      {
        data: { plan: "linux pro - debian",
                status: "delivered"},
        next: []
      }
    end

    get "/hosting/zabbix" do
      {
        data: { cluster_info: "up and running without any problem",
                server_info:  { ip: "192.01.12.03.11",
                                vlan: "vl002",
                                last_stop_at: "01/05/2011" }},
        next: []
      }
    end

    get "/hosting/product" do
      {
        data: { plan: "pro windows",
                status: "delivered"},
        next: []
      }
    end

    get "/sqlserver/zabbix" do
      {
        data: { cluster_info: "up and running without any problem",
                server_info:  { ip: "192.11.14.11.02",
                                vlan: "vl002",
                                last_stop_at: "01/04/2011" }},
        next: []
      }
    end
  end
end
