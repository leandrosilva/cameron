# -*- encoding: utf-8 -*-

# Foo is an awesome fake process used to test stuffs.
#
Rack::API.app do
  prefix     "foo"
  respond_to :json
  
  version "v0.0.1" do
    post "/start" do
      body = request.body.read
      payload = JSON.parse(body)
      
      puts "[Request :: start] #{payload}"
      
      {
        process:   "foo",
        name:      "whois",
        requestor: payload["requestor"],
        
        data:     { who_id:       payload["key"],
                    who_name:     "Leandro Silva",
                    who_login:    "leandrosilva.codezone",
                    who_web_info: { blog:    "http://leandrosilva.com.br",
                                    twitter: "codezone" },
                    who_dev_info: { github:  "http://github.com/leandrosilva" }},
                                             
        next_activities:
        {
          parallelizable: "yes",
        
          definitions:
          [
            { name: "act_1",
              url:  "http://localhost:9292/foo/v0.0.1/activity/act_1" },
            { name: "act_2",
              url:  "http://localhost:9292/foo/v0.0.1/activity/act_2" },
            { name: "act_3",
              url:  "http://localhost:9292/foo/v0.0.1/activity/act_3" },
            { name: "act_4",
              url:  "http://localhost:9292/foo/v0.0.1/activity/act_4" },
            { name: "act_5",
              url:  "http://localhost:9292/foo/v0.0.1/activity/act_5" }
          ]
        }
      }
    end
    
    post "/activity/act_1" do
      body = request.body.read
      payload = JSON.parse(body)
      
      puts "[Request :: act_1] #{payload}"

      {
        process:  "foo",
        name:     "foo_act_1",
        requestor: payload["requestor"],

        data: { bar: "the likable bar",
                baz:  { qux:   "the awesome qux",
                        quux:  "the amazing quux",
                        corge: "the great corge" }}
      }
    end
    
    post "/activity/act_2" do
      body = request.body.read
      payload = JSON.parse(body)
      
      puts "[Request :: act_2] #{payload}"

      {
        process:  "foo",
        name:     "foo_act_2",
        requestor: payload["requestor"],

        data: { bar: "the likable bar",
                baz:  { qux:   "the awesome qux",
                        quux:  "the amazing quux",
                        corge: "the great corge" }}
      }
    end

    post "/activity/act_3" do
      body = request.body.read
      payload = JSON.parse(body)
      
      puts "[Request :: act_3] #{payload}"

      {
        process:  "foo",
        name:     "foo_act_3",
        requestor: payload["requestor"],

        data: { bar: "the likable bar",
                baz:  { qux:   "the awesome qux",
                        quux:  "the amazing quux",
                        corge: "the great corge" }}
      }
    end

    post "/activity/act_4" do
      body = request.body.read
      payload = JSON.parse(body)
      
      puts "[Request :: act_4] #{payload}"

      {
        process:  "foo",
        name:     "foo_act_4",
        requestor: payload["requestor"],

        data: { bar: "the likable bar",
                baz:  { qux:   "the awesome qux",
                        quux:  "the amazing quux",
                        corge: "the great corge" }}
      }
    end

    post "/activity/act_5" do
      body = request.body.read
      payload = JSON.parse(body)
      
      puts "[Request :: act_5] #{payload}"

      {
        process:  "foo",
        name:     "foo_act_4",
        requestor: payload["requestor"],

        data: { bar: "the likable bar",
                baz:  { qux:   "the awesome qux",
                        quux:  "the amazing quux",
                        corge: "the great corge" }},
                                
        next_activities:
        {
          parallelizable: "yes",

          definitions:
          [
            { name: "act_5_sub_1",
              url:  "http://localhost:9292/foo/v0.0.1/activity/act_5_sub_1" }
          ]
        }
      }
    end
    
    post "/activity/act_5_sub_1" do
      body = request.body.read
      payload = JSON.parse(body)

      puts "[Request :: act_5_sub_1] #{payload}"

      {
        process:  "foo",
        name:     "foo_act_5_sub_1",
        requestor: payload["requestor"],

        data: { grault: "the likable grault",
                garply:  { waldo:   "the awesome waldo",
                           fred:  "the amazing fred",
                           plugh: "the great plugh" }}
      }
    end
  end
end
